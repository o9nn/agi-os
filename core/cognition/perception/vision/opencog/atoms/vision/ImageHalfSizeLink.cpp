#include "ImageHalfSizeLink.hpp"
#include "ImageNode.hpp"
#include "ImageValue.hpp"
#include <memory>
#include <opencog/atoms/base/ClassServer.h>
#include <opencog/atoms/core/NumberNode.h>
#include <opencog/atoms/flow/ValueOfLink.h>
#include <opencog/atoms/value/Value.h>
#include <opencv2/core/types.hpp>
#include <opencv2/imgproc.hpp>
using namespace opencog;
ImageHalfSizeLink::ImageHalfSizeLink(HandleSeq oset, Type t) :
FunctionLink(std::move(oset), t) {
init();
}
void ImageHalfSizeLink::init() {
Type tscope = get_type();
if (not nameserver().isA(tscope, IMAGE_HALF_SIZE_LINK))
throw InvalidParamException(TRACE_INFO,
"Expecting an ImageHalfSizeLink.");
if (getOutgoingSet().size() != 1)
throw InvalidParamException(TRACE_INFO,
"Wrong number of arguments, expecting 1.");
}
ValuePtr ImageHalfSizeLink::execute(AtomSpace* atomspace, bool silent) {
const Handle& arg1 = getOutgoingSet().at(0);
Type arg1_type = arg1.const_atom_ptr()->get_type();
if (not(nameserver().isA(arg1_type, IMAGE_NODE) or
nameserver().isA(arg1_type, VALUE_OF_LINK)))
throw InvalidParamException(TRACE_INFO,
"Wrong argument type on position 1, "
"expecting ImageNode or ValueOf.");
ImageValuePtr img_vp = nullptr;
auto img_atm = ImageNodeCast(arg1);
auto img_vof = ValueOfLinkCast(arg1);
if (img_vof != nullptr)
img_vp = ImageValueCast(img_vof->execute(atomspace, silent));
if (img_atm == nullptr and img_vp == nullptr)
throw InvalidParamException(
TRACE_INFO, "Invalid arguments: could not get valid input.");
const cv::Mat& input =
img_atm != nullptr ? img_atm->image() : img_vp->image();
cv::Mat output;
cv::pyrDown(input, output);
return createImageValue(output);
}
DEFINE_LINK_FACTORY(ImageHalfSizeLink, IMAGE_HALF_SIZE_LINK)