package types
import (
	"bytes"
	"encoding/json"
	"io"
	"strings"
	"github.com/moeru-ai/unspeech/pkg/apierrors"
	"github.com/samber/lo"
	"github.com/samber/mo"
)
type OpenAISpeechRequestOptions struct {
	Model string `json:"model"`
	Input string `json:"input"`
	Voice string `json:"voice"`
	ResponseFormat string `json:"response_format,omitempty"`
	Speed int `json:"speed,omitempty"`
	ExtraBody map[string]any `json:"extra_body,omitempty"`
}
type SpeechRequestOptions struct {
	OpenAISpeechRequestOptions
	Backend string `json:"backend"`
	Model   string `json:"model"`
	body          mo.Option[*bytes.Buffer]
	bodyParsedMap map[string]any
}
func (o SpeechRequestOptions) AsBuffer() mo.Option[*bytes.Buffer] {
	return o.body
}
func (o SpeechRequestOptions) AsMap() map[string]any {
	return o.bodyParsedMap
}
func NewSpeechRequestOptions(body io.ReadCloser) mo.Result[SpeechRequestOptions] {
	buffer := new(bytes.Buffer)
	_, err := buffer.ReadFrom(body)
	if err != nil {
		return mo.Err[SpeechRequestOptions](apierrors.NewErrBadRequest().WithDetail(err.Error()))
	}
	var optionsMap map[string]any
	err = json.Unmarshal(buffer.Bytes(), &optionsMap)
	if err != nil {
		return mo.Err[SpeechRequestOptions](apierrors.NewErrBadRequest().WithDetail(err.Error()))
	}
	var options OpenAISpeechRequestOptions
	err = json.Unmarshal(buffer.Bytes(), &options)
	if err != nil {
		return mo.Err[SpeechRequestOptions](apierrors.NewErrBadRequest().WithDetail(err.Error()))
	}
	if options.Model == "" || options.Input == "" || options.Voice == "" {
		return mo.Err[SpeechRequestOptions](apierrors.NewErrInvalidArgument().WithDetail("either one of model, input, and voice parameter is required"))
	}
	backendAndModel := lo.Ternary(
		strings.Contains(options.Model, "/"),
		strings.SplitN(options.Model, "/", 2), 
		[]string{options.Model, ""},
	)
	return mo.Ok(SpeechRequestOptions{
		OpenAISpeechRequestOptions: options,
		Backend:                    backendAndModel[0],
		Model:                      backendAndModel[1],
		body:                       mo.Some(buffer),
		bodyParsedMap:              optionsMap,
	})
}