import svgwrite
import math

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

# Helper function to create arrow markers
def create_arrow_marker(dwg, marker_id):
  insert = (3,3)
  size = (6, 6)
  path_d = "M0,0 L6,3 L0,6 Z"

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
  factor = 50 / length

  # Calculate the new start and end points
  start_x = center_x + direction_x * factor
  start_y = center_y + direction_y * factor
  end_x = end_x - direction_x * factor
  end_y = end_y - direction_y * factor
  
  dwg.add(
    dwg.line(
      start=(start_x, start_y),
      end=(end_x, end_y),
      stroke="black", 
      stroke_width=1.5, 
      marker_start=start_marker.get_funciri(),
      marker_end=end_marker.get_funciri()
    )
  )

# Create an SVG drawing
dwg = svgwrite.Drawing('ring_with_arrows.svg', profile='full', size=("500px", "500px"))
center_x = 250
center_y = 250

# Create arrow markers using the helper function
start_marker = create_arrow_marker(dwg, "start_arrow")
end_marker = create_arrow_marker(dwg, "end_arrow")

# Draw the central blue human
draw_human(dwg, center_x, center_y, "blue")

# Define the positions of surrounding grey humans in a circle
n = 8  # Number of humans in the circle
radius = 150  # Radius of the circle
for i in range(n):
  angle = (2 * math.pi / n) * i
  x = center_x + radius * math.cos(angle)
  y = center_y + radius * math.sin(angle)
  
  # Draw grey human at calculated position
  draw_human(dwg, x, y, "grey")
  
  # Draw double-sided arrow from center to grey human
  draw_double_arrow(
    dwg, 
    center_x, 
    center_y, 
    x, y, 
    start_marker, 
    end_marker
  )

# Save the drawing
dwg.save()
