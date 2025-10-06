# Oyster Measurement Analysis Summary

## Overview
Automated analysis of oyster images from outplant/sequim/size directory using computer vision techniques.

## Analysis Details
- **Date of Analysis**: 2025-01-27
- **Total Images Analyzed**: 8
- **Total Oysters Measured**: 1,007 total oysters across all images
- **Date in Images**: 20250626 (June 26, 2025)

## Image Files Analyzed
1. polyic_outplant_tag013_20250626.jpeg
2. polyic_outplant_tag014_20250626.jpeg
3. polyic_outplant_tag015_20250626.jpeg
4. polyic_outplant_tag016_20250626.jpeg
5. polyic_outplant_tag017_20250626.jpeg
6. polyic_outplant_tag018_20250626.jpeg
7. polyic_outplant_tag019_20250626.jpeg
8. polyic_outplant_tag020_20250626.jpeg

## Measurement Summary

### Overall Statistics
- **Average Length**: 14.53 mm
- **Average Width**: 8.62 mm
- **Total Measurements**: 1,007 oysters

### Summary by Tag
| Tag | Count | Length (mm) | Width (mm) |
|-----|-------|-------------|------------|
|     |       | Mean ± Std  | Mean ± Std |
| 013 | 124   | 13.00 ± 5.70 | 7.84 ± 3.14 |
| 014 | 112   | 13.69 ± 6.61 | 8.37 ± 3.88 |
| 015 | 116   | 13.97 ± 5.31 | 8.59 ± 3.93 |
| 016 | 125   | 14.42 ± 6.07 | 8.62 ± 3.81 |
| 017 | 134   | 16.29 ± 12.02 | 8.84 ± 4.81 |
| 018 | 130   | 13.59 ± 5.73 | 8.43 ± 3.48 |
| 019 | 135   | 15.60 ± 12.45 | 9.16 ± 6.43 |
| 020 | 131   | 15.35 ± 11.65 | 9.00 ± 5.34 |

## Data Structure
The CSV file contains the following columns:
- **Image**: Filename of the analyzed image
- **Date**: Date extracted from filename (20250626)
- **Tag**: Green tag ID (013-020)
- **Oyster**: Oyster identifier (oyster1-oysterN, where N varies by image)
- **Length**: Longest length measurement in mm
- **Width**: Longest width measurement in mm

## Technical Notes
- **Detection Method**: Computer vision contour detection with area and aspect ratio filtering
- **Measurement Method**: Rotated bounding rectangle analysis
- **Pixel to mm Conversion**: Estimated ratio of 0.1 (may need calibration with known reference)
- **Quality Control**: All detected objects per image, sorted by size
- **Annotated Images**: Generated with bounding boxes and oyster numbers for verification

## Files Generated
1. `oyster_measurements.csv` - Main data file with all measurements
2. `annotated/` - Directory containing images with detection overlays
3. `analysis_summary.md` - This summary report

## Recommendations
1. **Calibration**: Consider calibrating pixel-to-mm ratio using known reference objects
2. **Manual Verification**: Review annotated images to ensure accurate detection
3. **Quality Control**: Some measurements show high variability - may need manual verification
4. **Statistical Analysis**: Consider additional analysis of size distributions and growth patterns
