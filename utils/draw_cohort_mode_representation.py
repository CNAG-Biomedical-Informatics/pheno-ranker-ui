import sys
import math
import random
import svgwrite

from draw_human import parse_human_svg, draw_human_svg
from draw_axes import draw_axes

mode = "both"
if len(sys.argv) > 1:
  mode = sys.argv[1]

def setup(filename):
  # Create an SVG drawing
  svg_size = 500

  dwg = svgwrite.Drawing(
    filename, 
    profile='full', 
    size=(svg_size, svg_size)
  )

  # pixels from the edge of the SVG canvas towards the center
  # increase the value to move the axes closer to human figures
  padding = 32
  draw_axes(dwg, svg_size, padding)
      
  # Parse the SVG file for the human figure
  circle_dict, body_path = parse_human_svg()

  return dwg, svg_size, padding, circle_dict, body_path

def intra_cohort_mode_representation(pats_count):
  
  dwg, svg_size, padding, circle_dict, body_path = setup(
    "intra_cohort_mode_representation.svg"
  )

  # Draw a scattered representation of the human figures
  for _ in range(pats_count):
    x = random.randint(padding, svg_size - padding)
    y = random.randint(padding, svg_size - padding)
    color = f"rgb({random.randint(0, 255)}, {random.randint(0, 255)}, {random.randint(0, 255)})"
    draw_human_svg(dwg, x, y, circle_dict, body_path, color)

  # Save the drawing
  dwg.save(
    pretty=True,
  )

def inter_cohort_mode_representation(pats_count):
  dwg, svg_size, padding, circle_dict, body_path = setup(
    "inter_cohort_mode_representation.svg"
  )

  # Define the three colors
  colors = ["orange", "violet", "cyan"]

  # Define a clustering factor to control how tight the figures will be within each quadrant
  clustering_factor = 0.3 # Increased clustering factor to allow more space within each quadrant

  # Assume human figure has a size (radius) for collision detection
  human_radius = 5  # Adjust this value based on the size of the human figures

  # Divide the canvas into larger quadrants with tighter clusters

  divisor = 3
  quadrants = [
    {"x_center": (svg_size // divisor), "y_center": (svg_size // divisor), "positions": []},  # Top-left quadrant
    {"x_center": (2 * svg_size // divisor), "y_center": (svg_size // divisor), "positions": []},       # Top-right quadrant
    {"x_center": (svg_size // divisor), "y_center": (2 * svg_size // divisor), "positions": []},       # Bottom-left quadrant
  ]

  # Calculate the radius of the clustering area
  cluster_radius = int((svg_size // 3) * clustering_factor / 2)

  def is_colliding(x, y, positions, radius):
    """
    Check if the new figure collides with existing figures in the cluster.
    """
    for pos in positions:
      if math.sqrt((x - pos[0])**2 + (y - pos[1])**2) < 2 * radius:
        return True
    return False

  # Draw a scattered representation of the human figures
  for i in range(pats_count):
    # Choose a color and corresponding quadrant
    color_index = i % len(colors)
    color = colors[color_index]
    quadrant = quadrants[color_index]
    
    # Find a non-colliding position
    max_attempts = 100  # Limit the number of attempts to avoid infinite loops
    attempts = 0
    while attempts < max_attempts:
      x = random.randint(
        max(padding, quadrant["x_center"] - cluster_radius), 
        min(svg_size - padding, quadrant["x_center"] + cluster_radius)
      )
      y = random.randint(
        max(padding, quadrant["y_center"] - cluster_radius), 
        min(svg_size - padding, quadrant["y_center"] + cluster_radius)
      )
      
      # Check for collision with existing figures
      if not is_colliding(x, y, quadrant["positions"], human_radius):
          break  # Found a non-colliding position
      attempts += 1
    
    # If we reach the maximum attempts without finding a position, skip this figure
    if attempts >= max_attempts:
      print("Could not find a non-colliding position for the figure.")
      continue

    # Add the position to the list of placed figures
    quadrant["positions"].append((x, y))
    
    # Draw the human figure at the non-colliding position
    draw_human_svg(dwg, x, y, circle_dict, body_path, color)

  # Save the drawing
  dwg.save(pretty=True)

pats_count = 18

if mode == "both":
  intra_cohort_mode_representation(pats_count)
  inter_cohort_mode_representation(pats_count)

elif mode == "intra":
  intra_cohort_mode_representation(pats_count)

elif mode == "inter":
  inter_cohort_mode_representation(pats_count)