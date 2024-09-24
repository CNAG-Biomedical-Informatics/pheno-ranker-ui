
def draw_ax(dwg, start, end, color="black", stroke_width=1):
    dwg.add(dwg.line(start=start, end=end, stroke=color, stroke_width=stroke_width))

def add_label(dwg, text, position, font_size, color, anchor="middle"):
    dwg.add(dwg.text(text, insert=position, font_size=font_size, fill=color, text_anchor=anchor))

def draw_axes(dwg, svg_size, padding, axis_color="black", axis_stroke_width=1):

  # Draw the X and Y axes
  start = (padding, svg_size - padding)
  x_end = (svg_size - padding, svg_size - padding)
  y_end = (padding, padding)

  for end in [x_end, y_end]:
    draw_ax(dwg, start, end, axis_color, axis_stroke_width)

  # Calculate positions for the labels
  x_axis_label_pos = ((padding + (svg_size - padding)) / 2, svg_size - padding + 20)
  y_axis_label_pos = (padding - 20, (padding + (svg_size - padding)) / 2)

  for label_pos in [x_axis_label_pos, y_axis_label_pos]:
    add_label(dwg, "ΔD", label_pos, 14, axis_color)