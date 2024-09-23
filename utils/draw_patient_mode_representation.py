import svgwrite
import math
import random
from xml.dom import minidom

def parse_human_svg():
  doc = minidom.parse("human.svg")

  # extract all the circle elements cx="147.037" cy="70" r="20.104"
  cx = doc.getElementsByTagName('circle')[0].getAttribute('cx')
  cy = doc.getElementsByTagName('circle')[0].getAttribute('cy')
  r = doc.getElementsByTagName('circle')[0].getAttribute('r')
  circle_string = f'<circle cx="{cx}" cy="{cy}" r="{r}" />'
 
  path_string = [path.getAttribute('d') for path
                  in doc.getElementsByTagName('path')]
  
  # strip the path string
  path_string = [path.replace("\n", "").replace(" ", "") for path in path_string]
  
  print("circle_string", circle_string)
  print("path_string", path_string)
  doc.unlink()
  
  return circle_string, path_string

def draw_human_svg(dwg, x, y, circle_strings, path_strings, color="grey", scale=0.1):
  # Group to contain the entire human SVG element
  group = dwg.g(transform=f"translate({x},{y}) scale({scale})", fill=color)

  group.add(dwg.circle(cx="147.037", cy="70", r="20.104"))
  # group.add(dwg.path(d=path_strings[0]))

  body_path = (
        "M98.402,158.679 L104.963,112.661 C106.644,101.598 116.398,92.253 126.261,92.253 H167.816 "
        "C177.678,92.253 187.431,101.599 189.114,112.661 L195.687,158.766 L177.734,169.911 "
        "C175.299,171.423 173.66,173.934 173.256,176.769 L162.956,249.147 "
        "C162.951,249.184 162.945,249.222 162.94,249.259 C162.697,251.122 161.041,252.485 160.27,252.485 "
        "H133.833 C133.062,252.485 131.406,251.123 131.163,249.259 C131.158,249.222 131.152,249.184 "
        "131.147,249.147 L120.845,176.769 C120.442,173.933 118.802,171.423 116.368,169.911 L98.402,158.679 Z"
    )
    
  group.add(dwg.path(d=body_path))
  dwg.add(group)

# Helper function to create arrow markers
def create_arrow_marker(dwg, marker_id, path_d):
  insert = (3,3)
  size = (6, 6)

  marker = dwg.marker(
    id=marker_id,
    insert=insert, 
    size=size, 
    orient="auto"
  )
  marker.add(
    dwg.path(
      d=path_d,
      fill="black"
    )
  )
  dwg.defs.add(marker)
  return marker

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
dwg = svgwrite.Drawing(
  'patient_mode_representation.svg', 
  profile='full', 
  size=("500px", "500px")
)
center_x = 250
center_y = 250

# Create arrow markers using the helper function
start_marker = create_arrow_marker(dwg, "start_arrow", "M6,0 L0,3 L6,6 Z")
end_marker = create_arrow_marker(dwg, "end_arrow", "M0,0 L6,3 L0,6 Z")

circle_strings, path_strings = parse_human_svg()

# Draw the central blue human
# draw_human(dwg, center_x, center_y, "blue")
draw_human_svg(
  dwg, 
  center_x, 
  center_y, 
  circle_strings,
  path_strings,
  color="blue"
) 

# Define the positions of surrounding grey humans in a circle
n = 18 # Number of humans in the circle

min_distance = 100  
max_distance = 200  

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
    circle_strings,
    path_strings,
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