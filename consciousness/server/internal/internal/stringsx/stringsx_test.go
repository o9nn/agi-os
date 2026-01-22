package stringsx
import (
	"cmp"
	"strings"
	"testing"
)
func TestCompareFold(t *testing.T) {
	tests := []struct {
		a, b string
	}{
		{"", ""},
		{"a", "a"},
		{"a", "A"},
		{"A", "a"},
		{"a", "b"},
		{"b", "a"},
		{"abc", "ABC"},
		{"ABC", "abc"},
		{"abc", "abd"},
		{"abd", "abc"},
		{"abc", "ab"},
		{"ab", "abc"},
		{"世界", "世界"},
		{"Hello世界", "hello世界"},
		{"世界Hello", "世界hello"},
		{"世界", "世界x"},
		{"世界x", "世界"},
		{"ß", "ss"},      
		{"ﬁ", "fi"},      
		{"Σ", "σ"},       
		{"İ", "i\u0307"}, 
		{"HelloWorld", "helloworld"},
		{"HELLOWORLD", "helloworld"},
		{"helloworld", "HELLOWORLD"},
		{"HelloWorld", "helloworld"},
		{"helloworld", "HelloWorld"},
		{" ", " "},
		{"1", "1"},
		{"123", "123"},
		{"!@#", "!@#"},
	}
	wants := []int{}
	for _, tt := range tests {
		got := CompareFold(tt.a, tt.b)
		want := cmp.Compare(strings.ToLower(tt.a), strings.ToLower(tt.b))
		if got != want {
			t.Errorf("CompareFold(%q, %q) = %v, want %v", tt.a, tt.b, got, want)
		}
		wants = append(wants, want)
	}
	if n := testing.AllocsPerRun(1000, func() {
		for i, tt := range tests {
			if CompareFold(tt.a, tt.b) != wants[i] {
				panic("unexpected")
			}
		}
	}); n > 0 {
		t.Errorf("allocs = %v; want 0", int(n))
	}
}