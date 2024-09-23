import svgwrite
import math
import random

# Define the offsets for different parts of the human figure in a dictionary
HUMAN_PARTS = {
    'body': {'start_offset': (0, -15), 'end_offset': (0, 0)},
    'left_arm': {'start_offset': (0, -10), 'end_offset': (-5, -5)},
    'right_arm': {'start_offset': (0, -10), 'end_offset': (5, -5)},
    'left_leg': {'start_offset': (0, 0), 'end_offset': (-5, 10)},
    'right_leg': {'start_offset': (0, 0), 'end_offset': (5, 10)},
}

# Create a function to draw a human figure using the dictionary to reduce redundancy
def draw_human(dwg, center_x, center_y, color):
  # Draw the head
  dwg.add(dwg.circle(center=(center_x, center_y - 20), r=5, fill=color))
  
  # Draw body parts using the HUMAN_PARTS dictionary
  for _, offsets in HUMAN_PARTS.items():
    start_x = center_x + offsets['start_offset'][0]
    start_y = center_y + offsets['start_offset'][1]
    end_x = center_x + offsets['end_offset'][0]
    end_y = center_y + offsets['end_offset'][1]
    
    dwg.add(
      dwg.line(
        start=(start_x, start_y),
        end=(end_x, end_y),
        stroke=color, 
        stroke_width=2
      )
    )

def draw_simple_human(dwg, center_x, center_y, color, scale=1.0):
    # Draw the head as a circle
    dwg.add(dwg.circle(center=(center_x, center_y - 20 * scale), r=5 * scale, fill='none', stroke=color, stroke_width=2))
    
    # Draw the body as a rounded rectangle (representing the torso)
    body_width = 10 * scale
    body_height = 20 * scale
    dwg.add(dwg.rect(insert=(center_x - body_width / 2, center_y - 20 * scale), 
                     size=(body_width, body_height), 
                     fill='none', stroke=color, stroke_width=2, rx=5 * scale, ry=5 * scale))

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

# Function to draw double-sided arrows
def draw_double_arrow(dwg, center_x, center_y, end_x, end_y, start_marker, end_marker):
   # Calculate the direction vector from the center to the surrounding human
  direction_x = end_x - center_x
  direction_y = end_y - center_y
  length = math.sqrt(direction_x**2 + direction_y**2)

  # Calculate the factor to shorten the arrow
  shorten_factor = 30 / length

  # Calculate the new start and end points
  start_x = center_x + direction_x * shorten_factor
  start_y = center_y + direction_y * shorten_factor
  end_x = end_x - direction_x * shorten_factor
  end_y = end_y - direction_y * shorten_factor
  
  line = dwg.add(
    dwg.line(
      start=(start_x, start_y),
      end=(end_x, end_y),
      stroke="black", 
      stroke_width=1.5, 
      marker_start=start_marker.get_funciri(),
      marker_end=end_marker.get_funciri()
    )
  )

  return line, start_x, start_y, end_x, end_y, length

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

# Draw the central blue human
draw_human(dwg, center_x, center_y, "blue")

# Define the positions of surrounding grey humans in a circle
n = 18 # Number of humans in the circle
# radius = 100  # Radius of the circle

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
  draw_human(dwg, x, y, "grey")
  
  # Draw double-sided arrow from center to grey human
  # line, start_x, start_y, end_x, end_y, length = draw_double_arrow(
  #   dwg, 
  #   center_x, 
  #   center_y, 
  #   x, y, 
  #   start_marker, 
  #   end_marker
  # )

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

