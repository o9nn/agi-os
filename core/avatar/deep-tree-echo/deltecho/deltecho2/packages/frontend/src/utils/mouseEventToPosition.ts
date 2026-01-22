export function mouseEventToPosition(event: React.MouseEvent): {
  x: number
  y: number
} {
  if (event.clientX > 0 && event.clientY > 0) {
    return {
      x: event.clientX,
      y: event.clientY,
    }
  }
  const boundingBox = (event.target as HTMLElement).getBoundingClientRect()
  return {
    x: boundingBox.x + boundingBox.width / 2,
    y: boundingBox.y + boundingBox.height / 2,
  }
}