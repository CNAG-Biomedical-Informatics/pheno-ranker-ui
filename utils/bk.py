# No longer needed functions are backuped here

def draw_human(dwg, center_x, center_y, color):
  
  # Define the offsets for different parts of the human figure in a dictionary
  HUMAN_PARTS = {
      'body': {'start_offset': (0, -15), 'end_offset': (0, 0)},
      'left_arm': {'start_offset': (0, -10), 'end_offset': (-5, -5)},
      'right_arm': {'start_offset': (0, -10), 'end_offset': (5, -5)},
      'left_leg': {'start_offset': (0, 0), 'end_offset': (-5, 10)},
      'right_leg': {'start_offset': (0, 0), 'end_offset': (5, 10)},
  }

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