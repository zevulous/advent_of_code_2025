#!/usr/bin/env python3
"""
Solution for the Elf Present Fitting Puzzle
============================================

This puzzle is a 2D bin packing / constraint satisfaction problem.
We need to determine how many regions can fit all their required shapes.

Shapes can be rotated (0°, 90°, 180°, 270°) and flipped (horizontally),
giving up to 8 unique orientations per shape.

The algorithm uses backtracking:
1. Try to place the first shape in all valid positions
2. For each placement, recursively try to place remaining shapes
3. If all shapes placed successfully, region is solvable
4. If no valid placement exists, backtrack and try next position

Usage:
    python3 solve.py input.txt
    # or
    cat input.txt | python3 solve.py
    
For RStudio: All print statements use flush=True for real-time output.
"""

import sys
import time
from typing import List, Dict, Set, Tuple, FrozenSet


def log(msg=""):
    """Print with flush for real-time output in RStudio."""
    print(msg, flush=True)


def parse_input(text: str):
    """
    Parse the puzzle input into shapes and regions.
    """
    log("=" * 60)
    log("PARSING INPUT FILE...")
    log("=" * 60)
    
    lines = text.strip().split('\n')
    log(f"  Read {len(lines)} lines from input")
    
    shapes = {}
    regions = []
    current_id = None
    current_lines = []
    
    for line in lines:
        line_stripped = line.strip()
        
        # Check if this is a region line
        if 'x' in line_stripped and ':' in line_stripped:
            before_colon = line_stripped.split(':')[0].strip()
            if 'x' in before_colon:
                if current_id is not None:
                    shapes[current_id] = current_lines
                    current_id = None
                    current_lines = []
                
                idx = line_stripped.index(':')
                dims = line_stripped[:idx].strip()
                w, h = map(int, dims.split('x'))
                counts = list(map(int, line_stripped[idx+1:].strip().split()))
                regions.append((w, h, counts))
                continue
        
        if line_stripped and line_stripped[0].isdigit() and ':' in line_stripped:
            if current_id is not None:
                shapes[current_id] = current_lines
            
            idx = line_stripped.index(':')
            current_id = int(line_stripped[:idx])
            rest = line_stripped[idx+1:].strip()
            current_lines = [rest] if rest else []
            
        elif current_id is not None and line_stripped:
            current_lines.append(line_stripped)
    
    if current_id is not None:
        shapes[current_id] = current_lines
    
    log(f"  Found {len(shapes)} unique shapes")
    log(f"  Found {len(regions)} regions to check")
    log()
    
    return shapes, regions


def shape_to_coords(shape_lines: List[str]) -> Set[Tuple[int, int]]:
    """
    Convert a shape's visual representation to a set of (row, col) coordinates.
    """
    coords = set()
    for r, line in enumerate(shape_lines):
        for c, ch in enumerate(line):
            if ch == '#':
                coords.add((r, c))
    return coords


def normalize(coords: Set[Tuple[int, int]]) -> FrozenSet[Tuple[int, int]]:
    """
    Normalize coordinates so the minimum row and column are both 0.
    """
    if not coords:
        return frozenset()
    
    min_r = min(r for r, c in coords)
    min_c = min(c for r, c in coords)
    
    return frozenset((r - min_r, c - min_c) for r, c in coords)


def rotate_90(coords: Set[Tuple[int, int]]) -> Set[Tuple[int, int]]:
    """
    Rotate coordinates 90 degrees clockwise.
    """
    return {(c, -r) for r, c in coords}


def flip_horizontal(coords: Set[Tuple[int, int]]) -> Set[Tuple[int, int]]:
    """
    Flip coordinates horizontally (mirror across vertical axis).
    """
    return {(r, -c) for r, c in coords}


def get_all_orientations(coords: Set[Tuple[int, int]]) -> List[FrozenSet[Tuple[int, int]]]:
    """
    Generate all unique orientations of a shape (up to 8).
    """
    orientations = set()
    current = coords
    
    for _ in range(4):
        orientations.add(normalize(current))
        orientations.add(normalize(flip_horizontal(current)))
        current = rotate_90(current)
    
    return list(orientations)


def solve_region(width: int, height: int, shapes_to_place: List[int],
                 all_orientations: Dict[int, List[FrozenSet[Tuple[int, int]]]],
                 region_num: int, verbose: bool = True) -> bool:
    """
    Determine if all shapes can be placed in a region using backtracking.
    """
    if not shapes_to_place:
        return True
    
    grid = [[False] * width for _ in range(height)]
    
    # Track progress
    attempts = [0]
    start_time = time.time()
    last_update = [start_time]
    
    def can_place(shape_coords: FrozenSet[Tuple[int, int]], 
                  start_r: int, start_c: int) -> bool:
        for r, c in shape_coords:
            actual_r = start_r + r
            actual_c = start_c + c
            
            if actual_r < 0 or actual_r >= height:
                return False
            if actual_c < 0 or actual_c >= width:
                return False
            
            if grid[actual_r][actual_c]:
                return False
        
        return True
    
    def place_shape(shape_coords: FrozenSet[Tuple[int, int]], 
                    start_r: int, start_c: int, occupied: bool):
        for r, c in shape_coords:
            grid[start_r + r][start_c + c] = occupied
    
    def backtrack(shape_index: int) -> bool:
        if shape_index >= len(shapes_to_place):
            return True
        
        shape_id = shapes_to_place[shape_index]
        
        for orientation in all_orientations[shape_id]:
            max_r = max(r for r, c in orientation)
            max_c = max(c for r, c in orientation)
            
            for start_r in range(height - max_r):
                for start_c in range(width - max_c):
                    attempts[0] += 1
                    
                    # Print progress every 5 seconds
                    if verbose and time.time() - last_update[0] > 5.0:
                        elapsed = time.time() - start_time
                        log(f"      ... still working: {attempts[0]:,} placements tried, "
                            f"placing shape {shape_index + 1}/{len(shapes_to_place)}, "
                            f"{elapsed:.1f}s elapsed")
                        last_update[0] = time.time()
                    
                    if can_place(orientation, start_r, start_c):
                        place_shape(orientation, start_r, start_c, True)
                        
                        if backtrack(shape_index + 1):
                            return True
                        
                        place_shape(orientation, start_r, start_c, False)
        
        return False
    
    result = backtrack(0)
    
    if verbose:
        elapsed = time.time() - start_time
        log(f"      Completed in {elapsed:.2f}s after {attempts[0]:,} placement attempts")
    
    return result


def solve(input_text: str) -> int:
    """
    Main solver function. Can be called from R with input text.
    Returns the number of solvable regions.
    """
    log()
    log("*" * 60)
    log("*  ELF PRESENT FITTING PUZZLE SOLVER                       *")
    log("*" * 60)
    log()
    
    # Parse input
    shapes_raw, regions = parse_input(input_text)
    
    # Display shapes
    log("=" * 60)
    log("ANALYZING SHAPES...")
    log("=" * 60)
    
    all_orientations = {}
    for shape_id, shape_lines in shapes_raw.items():
        coords = shape_to_coords(shape_lines)
        orientations = get_all_orientations(coords)
        all_orientations[shape_id] = orientations
        
        num_cells = len(coords)
        log(f"  Shape {shape_id}: {num_cells} cells, {len(orientations)} unique orientations")
    log()
    
    # Process regions
    log("=" * 60)
    log("CHECKING REGIONS...")
    log("=" * 60)
    log()
    
    solvable_count = 0
    total_regions = len(regions)
    
    for region_idx, (width, height, counts) in enumerate(regions):
        region_num = region_idx + 1
        log(f"Region {region_num}/{total_regions}: {width}x{height} grid ({width * height} cells)")
        
        # Build list of shapes to place
        shapes_to_place = []
        shape_summary = []
        for shape_id, quantity in enumerate(counts):
            if quantity > 0:
                shapes_to_place.extend([shape_id] * quantity)
                shape_summary.append(f"{quantity}x shape{shape_id}")
        
        if shape_summary:
            log(f"  Shapes needed: {', '.join(shape_summary)}")
        else:
            log(f"  Shapes needed: none")
        
        log(f"  Total shapes to place: {len(shapes_to_place)}")
        
        # Calculate cells needed
        total_cells_needed = sum(
            len(list(all_orientations[sid])[0]) for sid in shapes_to_place
        )
        total_cells_available = width * height
        fill_percentage = (total_cells_needed / total_cells_available * 100) if total_cells_available > 0 else 0
        
        log(f"  Cells needed: {total_cells_needed} / {total_cells_available} available ({fill_percentage:.1f}% fill)")
        
        # Quick check
        if total_cells_needed > total_cells_available:
            log(f"  [X] IMPOSSIBLE - Not enough space!")
            log()
            continue
        
        if not shapes_to_place:
            log(f"  [OK] TRIVIAL - No shapes to place!")
            solvable_count += 1
            log()
            continue
        
        # Sort shapes by size (largest first) for better pruning
        shapes_to_place.sort(
            key=lambda sid: -len(list(all_orientations[sid])[0])
        )
        
        log(f"  Solving (largest shapes first)...")
        
        # Try to solve
        if solve_region(width, height, shapes_to_place, all_orientations, region_num):
            solvable_count += 1
            log(f"  [OK] SUCCESS - All shapes fit!")
        else:
            log(f"  [X] FAILED - Cannot fit all shapes")
        
        log()
    
    # Final summary
    log("=" * 60)
    log("FINAL RESULTS")
    log("=" * 60)
    log(f"  Regions that CAN fit all shapes: {solvable_count} / {total_regions}")
    log()
    log("=" * 60)
    log(f"ANSWER: {solvable_count}")
    log("=" * 60)
    log()
    
    return solvable_count


def main():
    """
    Main entry point for command-line usage.
    """
    # Read input
    if len(sys.argv) > 1:
        input_file = sys.argv[1]
        log(f"Reading input from: {input_file}")
        with open(input_file, 'r') as f:
            text = f.read()
    else:
        log("Reading input from stdin...")
        text = sys.stdin.read()
    
    result = solve(text)
    
    # Also print just the number for easy parsing
    print(result)


if __name__ == "__main__":
    # Automatically use day_twelve_test_input.txt
    input_file = "day_twelve_input.txt"
    log(f"Reading input from: {input_file}")
    with open(input_file, 'r') as f:
        text = f.read()
    result = solve(text)
    print(result)

