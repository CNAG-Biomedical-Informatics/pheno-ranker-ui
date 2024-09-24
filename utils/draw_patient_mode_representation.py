import math
import random
import svgwrite

from draw_human import parse_human_svg, draw_human_svg
from draw_axes import draw_axes

# Function to draw a line with vertical bars (|-------|)
def draw_barred_line(dwg, x1, y1, x2, y2, shorten_by=20, color="black"):
  # Calculate the direction vector from the center to the surrounding human
  dx = x2 - x1
  dy = y2 - y1
  length = math.sqrt(dx**2 + dy**2)
  
  # Calculate the factor to shorten the line
  shorten_factor = shorten_by / length
  
  # Calculate the new start and end points
  new_x1 = x1 + dx * shorten_factor
  new_y1 = y1 + dy * shorten_factor
  new_x2 = x2 - dx * shorten_factor
  new_y2 = y2 - dy * shorten_factor
  
  # Draw the main line
  line = dwg.add(dwg.line(start=(new_x1, new_y1), end=(new_x2, new_y2), stroke=color, stroke_width=1.5))
  
  # Calculate the perpendicular direction vector for the vertical bars
  perp_dx = -dy / length
  perp_dy = dx / length
  
  # Define the length of the vertical bars
  bar_length = 5
  
  # Draw vertical bar at the start
  bar_start_x1 = new_x1 + perp_dx * bar_length / 2
  bar_start_y1 = new_y1 + perp_dy * bar_length / 2
  bar_start_x2 = new_x1 - perp_dx * bar_length / 2
  bar_start_y2 = new_y1 - perp_dy * bar_length / 2
  dwg.add(dwg.line(start=(bar_start_x1, bar_start_y1), end=(bar_start_x2, bar_start_y2), stroke=color, stroke_width=1.5))
  
  # Draw vertical bar at the end
  bar_end_x1 = new_x2 + perp_dx * bar_length / 2
  bar_end_y1 = new_y2 + perp_dy * bar_length / 2
  bar_end_x2 = new_x2 - perp_dx * bar_length / 2
  bar_end_y2 = new_y2 - perp_dy * bar_length / 2
  dwg.add(dwg.line(start=(bar_end_x1, bar_end_y1), end=(bar_end_x2, bar_end_y2), stroke=color, stroke_width=1.5))
  
  return line, new_x1, new_y1, new_x2, new_y2, length

# Create an SVG drawing
svg_size = 500

dwg = svgwrite.Drawing(
  'patient_mode_representation.svg', 
  profile='full', 
  size=(svg_size, svg_size),
  viewBox=f"0 0 {svg_size} {svg_size}"
)

# pixels from the edge of the SVG canvas towards the center
# increase the value to move the axes closer to human figures
padding = 30 
draw_axes(dwg, svg_size, padding)
    
# Parse the SVG file for the human figure
circle_dict, body_path = parse_human_svg()

# Draw the central blue human
center_x = svg_size / 2
center_y = svg_size / 2
vertical_offset = 10

draw_human_svg(
  dwg, 
  center_x, 
  center_y - vertical_offset, 
  circle_dict,
  body_path,
  color="blue"
) 

# Define the positions of surrounding grey humans in a circle
n = 18 # Number of humans in the circle

min_distance = svg_size / 4
max_distance = svg_size / 2.2

# min_distance = 50  
# max_distance = 100 

# Track shortest distance information
shortest_distance = float('inf')
shortest_line_info = None

# List to keep all lines information
lines_info = []

for i in range(n):
  angle = (2 * math.pi / n) * i
  rnd_radius = random.uniform(min_distance, max_distance)
  
  x = center_x + rnd_radius * math.cos(angle)
  y = center_y + rnd_radius * math.sin(angle)
  
  # Draw grey human at calculated position
  draw_human_svg(
    dwg, x,y,
    circle_dict,
    body_path,
    color="grey"
  )
  
  line, start_x, start_y, end_x, end_y, length = draw_barred_line(
    dwg, center_x, center_y, x, y, shorten_by=20
  )

  lines_info.append(
    (line, start_x, start_y, end_x, end_y, length)
  )

  if length < shortest_distance:
    shortest_distance = length
    shortest_line_info = (
      line, start_x, start_y, end_x, end_y, length
    )

# Highlight the shortest line in blue and add a checkmark
if shortest_line_info:
  # Unpack shortest line info
  shortest_line, sx1, sy1, sx2, sy2 = shortest_line_info[:5]
  
  # Update the color of the shortest line to blue
  shortest_line.stroke(color="blue", width=1.5)
  
  # Add a checkmark in the middle of the shortest line
  check_x = (sx1 + sx2) / 2
  check_y = (sy1 + sy2) / 2
  dwg.add(dwg.text(
    "✓", 
    insert=(check_x, check_y),
    fill="blue",
    font_size="15px", 
    font_weight="bold"
  ))

# Save the drawing
dwg.save(
  pretty=True,
)