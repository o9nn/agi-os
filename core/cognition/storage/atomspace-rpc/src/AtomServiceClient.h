#ifndef ATOMSPACE_SERVER_ATOMSERVICECLIENT_H
#define ATOMSPACE_SERVER_ATOMSERVICECLIENT_H
#include <chrono>
#include <iostream>
#include <memory>
#include <string>
#include <opencog/atoms/base/Handle.h>
#include <opencog/atoms/base/Node.h>
#include <opencog/atoms/base/Link.h>
#include <opencog/bioscience/types/atom_types.h>
#include <opencog/atomspace/AtomSpace.h>
#include <grpcpp/channel.h>
#include <grpcpp/client_context.h>
#include <grpcpp/create_channel.h>
#include <grpcpp/security/credentials.h>
#include "atom_server.grpc.pb.h"
using namespace opencog;
using grpc::Channel;
using grpc::ClientContext;
using grpc::ClientReader;
using grpc::ClientReaderWriter;
using grpc::ClientWriter;
using grpc::Status;
using grpc::StatusCode;
class AtomServiceClient {
public:
AtomServiceClient(const std::string& address):
_stub(AtomServer::NewStub(grpc::CreateChannel(address, grpc::InsecureChannelCredentials())))
{}
explicit AtomServiceClient(const std::shared_ptr<grpc::Channel>& channel) :
_stub(AtomServer::NewStub(channel))
{}
void ExecutePattern(const std::string &atom_id, const Handle &patt, AtomSpace* as, HandleSeq&
result);
Handle CheckNode(const std::string &atom_id, const std::string &type_name, const std::string
&node_name);
void FindSimilar(const std::string &atom_id, const std::string &type_name, const std::string
&node_name, HandleSeq& result, AtomSpace* as);
NodeMsg FindType(const std::string &atom_id, const std::string &name);
private:
Handle FromNodeMsg(const NodeMsg& node);
Handle FromLinkMsg(const LinkMsg& link);
std::unique_ptr<AtomServer::Stub> _stub;
};
#endif