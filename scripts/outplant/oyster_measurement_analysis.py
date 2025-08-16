#!/usr/bin/env python3
"""
Oyster Measurement Analysis Script
Analyzes oyster images from outplant/sequim/size directory and extracts measurements
"""

import cv2
import numpy as np
import pandas as pd
import os
import re
from pathlib import Path
import matplotlib.pyplot as plt
from matplotlib.patches import Rectangle
import json

class OysterAnalyzer:
    def __init__(self, image_dir):
        self.image_dir = Path(image_dir)
        self.results = []
        
    def extract_tag_from_filename(self, filename):
        """Extract tag number from filename"""
        match = re.search(r'tag(\d+)', filename)
        return match.group(1) if match else "unknown"
    
    def extract_date_from_filename(self, filename):
        """Extract date from filename"""
        match = re.search(r'(\d{8})', filename)
        return match.group(1) if match else "unknown"
    
    def preprocess_image(self, image):
        """Preprocess image for oyster detection"""
        # Convert to grayscale
        gray = cv2.cvtColor(image, cv2.COLOR_BGR2GRAY)
        
        # Apply Gaussian blur to reduce noise
        blurred = cv2.GaussianBlur(gray, (5, 5), 0)
        
        # Apply adaptive thresholding
        thresh = cv2.adaptiveThreshold(blurred, 255, cv2.ADAPTIVE_THRESH_GAUSSIAN_C, 
                                     cv2.THRESH_BINARY_INV, 11, 2)
        
        # Morphological operations to clean up the image
        kernel = np.ones((3, 3), np.uint8)
        thresh = cv2.morphologyEx(thresh, cv2.MORPH_CLOSE, kernel)
        thresh = cv2.morphologyEx(thresh, cv2.MORPH_OPEN, kernel)
        
        return thresh
    
    def detect_oysters(self, image, thresh):
        """Detect potential oyster contours"""
        # Find contours
        contours, _ = cv2.findContours(thresh, cv2.RETR_EXTERNAL, cv2.CHAIN_APPROX_SIMPLE)
        
        # Filter contours by area and aspect ratio
        oyster_contours = []
        for contour in contours:
            area = cv2.contourArea(contour)
            if area > 1000:  # Minimum area threshold
                x, y, w, h = cv2.boundingRect(contour)
                aspect_ratio = w / h if h > 0 else 0
                if 0.5 < aspect_ratio < 2.0:  # Reasonable oyster aspect ratio
                    oyster_contours.append(contour)
        
        return oyster_contours
    
    def measure_oyster(self, contour, pixel_to_mm_ratio=0.1):
        """Measure length and width of an oyster contour"""
        # Get bounding rectangle
        x, y, w, h = cv2.boundingRect(contour)
        
        # Get rotated bounding rectangle for more accurate measurements
        rect = cv2.minAreaRect(contour)
        box = cv2.boxPoints(rect)
        box = np.int0(box)
        
        # Calculate dimensions
        width = min(rect[1])
        height = max(rect[1])
        
        # Convert to mm (assuming pixel_to_mm_ratio)
        length_mm = height * pixel_to_mm_ratio
        width_mm = width * pixel_to_mm_ratio
        
        return length_mm, width_mm, (x, y, w, h)
    
    def analyze_image(self, image_path):
        """Analyze a single image and extract oyster measurements"""
        print(f"Analyzing {image_path.name}...")
        
        # Read image
        image = cv2.imread(str(image_path))
        if image is None:
            print(f"Could not read image: {image_path}")
            return []
        
        # Preprocess image
        thresh = self.preprocess_image(image)
        
        # Detect oysters
        oyster_contours = self.detect_oysters(image, thresh)
        
        # Sort contours by area (largest first) - measure all detected oysters
        oyster_contours = sorted(oyster_contours, key=cv2.contourArea, reverse=True)
        
        print(f"Found {len(oyster_contours)} oysters in {image_path.name}")
        
        # Measure each oyster
        measurements = []
        for i, contour in enumerate(oyster_contours):
            length_mm, width_mm, bbox = self.measure_oyster(contour)
            
            measurement = {
                'Image': image_path.name,
                'Date': self.extract_date_from_filename(image_path.name),
                'Tag': self.extract_tag_from_filename(image_path.name),
                'Oyster': f'oyster{i+1}',
                'Length': round(length_mm, 2),
                'Width': round(width_mm, 2)
            }
            measurements.append(measurement)
            
            # Draw bounding box on image for visualization
            x, y, w, h = bbox
            cv2.rectangle(image, (x, y), (x + w, y + h), (0, 255, 0), 2)
            cv2.putText(image, f'{i+1}', (x, y-10), cv2.FONT_HERSHEY_SIMPLEX, 0.5, (0, 255, 0), 1)
        
        # Save annotated image
        output_dir = self.image_dir / 'annotated'
        output_dir.mkdir(exist_ok=True)
        annotated_path = output_dir / f"annotated_{image_path.name}"
        cv2.imwrite(str(annotated_path), image)
        
        return measurements
    
    def analyze_all_images(self):
        """Analyze all images in the directory"""
        image_files = list(self.image_dir.glob('*.jpeg')) + list(self.image_dir.glob('*.jpg'))
        
        all_measurements = []
        for image_file in sorted(image_files):
            measurements = self.analyze_image(image_file)
            all_measurements.extend(measurements)
        
        return all_measurements
    
    def save_results(self, measurements, output_file):
        """Save results to CSV file"""
        df = pd.DataFrame(measurements)
        df.to_csv(output_file, index=False)
        print(f"Results saved to {output_file}")
        
        # Print summary
        print(f"\nSummary:")
        print(f"Total images analyzed: {df['Image'].nunique()}")
        print(f"Total oysters measured: {len(df)}")
        print(f"Average length: {df['Length'].mean():.2f} mm")
        print(f"Average width: {df['Width'].mean():.2f} mm")
        
        return df

def main():
    # Set up paths
    image_dir = Path("data/outplant/sequim/size")
    output_file = image_dir / "oyster_measurements.csv"
    
    # Create analyzer
    analyzer = OysterAnalyzer(image_dir)
    
    # Analyze all images
    print("Starting oyster measurement analysis...")
    measurements = analyzer.analyze_all_images()
    
    if measurements:
        # Save results
        df = analyzer.save_results(measurements, output_file)
        
        # Create summary by tag
        print("\nSummary by tag:")
        tag_summary = df.groupby('Tag').agg({
            'Oyster': 'count',
            'Length': ['mean', 'std'],
            'Width': ['mean', 'std']
        }).round(2)
        print(tag_summary)
        
    else:
        print("No measurements were extracted. Please check the images and parameters.")

if __name__ == "__main__":
    main()
