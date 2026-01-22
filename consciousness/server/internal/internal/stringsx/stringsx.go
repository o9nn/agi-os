package stringsx
import (
	"unicode"
	"unicode/utf8"
)
func CompareFold(a, b string) int {
	ia, ib := 0, 0
	for ia < len(a) && ib < len(b) {
		ra, wa := nextRuneLower(a[ia:])
		rb, wb := nextRuneLower(b[ib:])
		if ra < rb {
			return -1
		}
		if ra > rb {
			return 1
		}
		ia += wa
		ib += wb
		if wa == 0 || wb == 0 {
			break
		}
	}
	switch {
	case ia == len(a) && ib == len(b):
		return 0
	case ia == len(a):
		return -1
	default:
		return 1
	}
}
func nextRuneLower(s string) (r rune, width int) {
	r, width = utf8.DecodeRuneInString(s)
	return unicode.ToLower(r), width
}