import re
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