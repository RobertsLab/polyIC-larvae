# Oyster Measurement Analysis Summary

## Overview
Automated analysis of oyster images from outplant/sequim/size directory using computer vision techniques.

## Analysis Details
- **Date of Analysis**: 2025-01-27
- **Total Images Analyzed**: 8
- **Total Oysters Measured**: 240 (30 per image)
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
- **Average Length**: 20.61 mm
- **Average Width**: 13.51 mm
- **Total Measurements**: 240 oysters

### Summary by Tag
| Tag | Count | Length (mm) | Width (mm) |
|-----|-------|-------------|------------|
|     |       | Mean ± Std  | Mean ± Std |
| 013 | 30    | 17.19 ± 5.76 | 11.46 ± 3.05 |
| 014 | 30    | 18.16 ± 8.86 | 12.16 ± 4.06 |
| 015 | 30    | 18.03 ± 4.20 | 12.88 ± 3.66 |
| 016 | 30    | 19.06 ± 5.09 | 13.22 ± 3.33 |
| 017 | 30    | 25.22 ± 16.44 | 14.80 ± 5.49 |
| 018 | 30    | 17.76 ± 5.91 | 12.56 ± 3.15 |
| 019 | 30    | 25.24 ± 22.44 | 15.45 ± 10.60 |
| 020 | 30    | 24.23 ± 19.67 | 15.54 ± 7.21 |

## Data Structure
The CSV file contains the following columns:
- **Image**: Filename of the analyzed image
- **Date**: Date extracted from filename (20250626)
- **Tag**: Green tag ID (013-020)
- **Oyster**: Oyster identifier (oyster1-oyster30)
- **Length**: Longest length measurement in mm
- **Width**: Longest width measurement in mm

## Technical Notes
- **Detection Method**: Computer vision contour detection with area and aspect ratio filtering
- **Measurement Method**: Rotated bounding rectangle analysis
- **Pixel to mm Conversion**: Estimated ratio of 0.1 (may need calibration with known reference)
- **Quality Control**: Top 30 largest detected objects per image
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
