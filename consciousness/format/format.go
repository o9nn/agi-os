package format
import (
"fmt"
"math"
"strconv"
)
const (
Thousand = 1000
Million  = Thousand * 1000
Billion  = Million * 1000
)
func HumanNumber(b uint64) string {
switch {
case b >= Billion:
number := float64(b) / Billion
if number == math.Floor(number) {
return fmt.Sprintf("%.0fB", number)
}
return fmt.Sprintf("%.1fB", number)
case b >= Million:
number := float64(b) / Million
if number == math.Floor(number) {
return fmt.Sprintf("%.0fM", number)
}
return fmt.Sprintf("%.2fM", number)
case b >= Thousand:
return fmt.Sprintf("%.0fK", float64(b)/Thousand)
default:
return strconv.FormatUint(b, 10)
}
}