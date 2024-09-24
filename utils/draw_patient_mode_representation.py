import re
import math
import random
import svgwrite
from svgpathtools import svg2paths2

def parse_human_svg():

  # Function to round the floating-point numbers to a reasonable precision
  def round_floats_in_path(path_string, precision=5):
    #find floating-point numbers
    float_regex = re.compile(r"-?\d+\.\d+")
    
    # Replace each match with a rounded version
    def round_match(match):
      return str(round(float(match.group()), precision))
    
    return float_regex.sub(round_match, path_string)


  # Read the SVG file using svgpathtools
  paths, attributes, _ = svg2paths2('human.svg')

  _,circle_dict = attributes
  circle_dict = {key: float(value) for key, value in circle_dict.items()}

  # Convert each path to a formatted SVG path
  formatted_paths = []
  for path in paths:
    path_string = ""
    
    # Start with the 'M' command to set the initial position
    start_segment = path[0]
    start_x, start_y = start_segment.start.real, start_segment.start.imag
    path_string += f"M{start_x},{start_y} "

    for segment in path:

      if segment.__class__.__name__ == 'Line':
        end_x, end_y = segment.end.real, segment.end.imag

        if start_x == end_x:  # Vertical line
            path_string += f"V{end_y} "
        elif start_y == end_y:  # Horizontal line
            path_string += f"H{end_x} "
        else:
            # Regular line command
            path_string += f"L{end_x},{end_y} "

      elif segment.__class__.__name__ == 'CubicBezier':
        path_string += (
          f"C{segment.control1.real},{segment.control1.imag} "
          f"{segment.control2.real},{segment.control2.imag} "
          f"{segment.end.real},{segment.end.imag} "
        )

      elif segment.__class__.__name__ == 'QuadraticBezier':
        path_string += (
          f"Q{segment.control.real},{segment.control.imag} "
          f"{segment.end.real},{segment.end.imag} "
        )

      elif segment.__class__.__name__ == 'Arc':
        path_string += (
          f"A{segment.radius.real},{segment.radius.imag} "
          f"{segment.rotation} {int(segment.large_arc)},{int(segment.sweep)} "
          f"{segment.end.real},{segment.end.imag} "
        )

    path_string += "Z"  # Close the path
    formatted_paths.append(path_string.strip())

  # Round the floating-point numbers in the path string
  body_path = round_floats_in_path(formatted_paths[0])
  
  return circle_dict, body_path

def draw_human_svg(dwg, x, y, circle_dict, body_path, color="grey", scale=0.1):
  # Calculate the offset to align the human figure correctly
  head_center_x = circle_dict['cx']
  head_center_y = circle_dict['cy']
  
  # Adjust the translation to center the human figure
  offset_x = x - head_center_x * scale
  offset_y = y - head_center_y * scale

  # Group to contain the entire human SVG element
  group = dwg.g(transform=f"translate({offset_x},{offset_y}) scale({scale})", fill=color)

   # Add the head (circle)
  group.add(
    dwg.circle(
      center=(head_center_x,head_center_y),
      r=circle_dict['r']
    )
  )
  
  # group.add(dwg.path(d=body_path))
  group.add(dwg.path(d=body_path))
  dwg.add(group)

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
  size=(svg_size, svg_size)
)
center_x = 250
center_y = 250

axis_color = "black"
axis_stroke_width = 1

# pixels from the edge of the SVG canvas towards the center
# increase the value to move the axes closer to human figures
padding = 140 
    
# Draw the axes
# X axis
dwg.add(
  dwg.line(
    start=(padding, svg_size - padding),
    end=(svg_size - padding, svg_size - padding),
    stroke=axis_color, stroke_width=axis_stroke_width
  )
)

# Y axis
dwg.add(
  dwg.line(
    start=(padding, svg_size - padding),
    end=(padding, padding),
    stroke=axis_color, 
    stroke_width=axis_stroke_width
  )
)


# Add axis labels "ΔD"
# Middle of the X axis
x_axis_label_x = (padding + (svg_size - padding)) / 2  # Middle X position
x_axis_label_y = svg_size - padding + 20  # Slightly below the X axis

dwg.add(dwg.text(
  "ΔD", insert=(x_axis_label_x, x_axis_label_y), 
  font_size=14, 
  fill=axis_color, 
  text_anchor="middle"
))

# Middle of the Y axis
y_axis_label_x = padding - 20  # Slightly left of the Y axis
y_axis_label_y = (padding + (svg_size - padding)) / 2  # Middle Y position
dwg.add(dwg.text(
  "ΔD", insert=(y_axis_label_x, y_axis_label_y),
  font_size=14, 
  fill=axis_color, 
  text_anchor="middle"
))

circle_dict, body_path = parse_human_svg()

# Draw the central blue human
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

min_distance = 50  
max_distance = 100 

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
  # draw_human(dwg, x, y, "grey")
  draw_human_svg(
    dwg, 
    x,
    y,
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