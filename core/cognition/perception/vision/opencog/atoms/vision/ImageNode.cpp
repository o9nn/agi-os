#include "ImageNode.hpp"
#include <mutex>
#include <opencog/atoms/atom_types/NameServer.h>
#include <opencog/atoms/vision/atom_types.h>
#include <opencog/util/Logger.h>
#include <opencv2/imgcodecs.hpp>
using namespace opencog;
ImageNode::ImageNode(Type t, const std::string& filename) :
Node(t, filename), _image(cv::imread(filename)) {}
const cv::Mat& ImageNode::image() const { return _image; }
DEFINE_NODE_FACTORY(ImageNode, IMAGE_NODE)