let server;
let endpoint;
let serverURL;
let errorState = "";
let socket;
let serverText;
let endpointMenu;
let endpointType;
let urlSpan;
let connectionSpan;
let connectButton;
let replySpan;
let outgoingText;
function setup()
{
serverText = document.getElementById('server-box');
endpointMenu = document.getElementById('endpoint-menu');
endpointType = document.getElementById('endpoint');
urlSpan = document.getElementById('full-url');
replySpan = document.getElementById('reply');
outgoingText = document.getElementById('outgoing');
connectionSpan = document.getElementById('connection');
connectButton = document.getElementById('connectButton');
serverText.addEventListener('change', setServer);
outgoingText.addEventListener('keydown', function(event) {
if (event.key === 'Enter' && !event.shiftKey) {
event.preventDefault();
sendMessage();
}
});
connectButton.addEventListener('click', changeConnection);
server = 'ws://localhost:18080/';
endpoint = 'json';
serverURL = server + endpoint;
serverText.value = server;
endpointMenu.value = endpoint;
urlSpan.innerHTML = serverURL;
openSocket(serverURL);
}
function setServer() {
server = serverText.value;
endpoint = endpointMenu.value;
serverURL = server + endpoint;
console.log("Enter setServer, new url=" + serverURL);
}
function openSocket(url)
{
socket = new WebSocket(url);
socket.addEventListener('open', openConnection);
socket.addEventListener('close', closeConnection);
socket.addEventListener('message', readReplyMessage);
socket.addEventListener('error', reportError);
}
function changeConnection(event)
{
console.log("button click; socket state=" + socket.readyState + " vs closed=" + WebSocket.CLOSED);
if (socket.readyState === WebSocket.CLOSED) {
server = serverText.value;
endpoint = endpointMenu.value;
serverURL = server + endpoint;
console.log("Opening socket connection to " + serverURL);
openSocket(serverURL);
} else {
console.log("close socket");
socket.close();
}
}
function openConnection()
{
errorState = "";
serverText.value = server;
urlSpan.innerHTML = serverURL;
connectionSpan.innerHTML = "true";
connectionSpan.className = "connected";
connectButton.value = "Disconnect";
if (endpoint == 'json')
endpointType.innerHTML = "JSON";
else if (endpoint == 'scm')
endpointType.innerHTML = "Guile Scheme";
else if (endpoint == 'py')
endpointType.innerHTML = "Python";
else if (endpoint == 'sexpr')
endpointType.innerHTML = "S-Expressions";
}
function closeConnection()
{
urlSpan.innerHTML = "none";
connectionSpan.innerHTML = "false" + errorState;
connectionSpan.className = "";
connectButton.value = "Connect";
endpointType.innerHTML = "none";
replySpan.innerHTML = "";
}
function reportError(event)
{
console.log("oh nooo=" + event.data + "<<");
errorState = "; Unable to connect to \'" + serverURL + "\'";
}
function readReplyMessage(event)
{
console.log("got reply=" + event.data + "<<");
replySpan.innerHTML = event.data;
}
function sendMessage(keepCommand)
{
console.log("enter sendmsg; socket state=" + socket.readyState + " vs open=" + WebSocket.OPEN);
if (socket.readyState === WebSocket.OPEN) {
winl = outgoingText.value;
console.log("going to send this" + winl + "<<");
socket.send(winl);
if (!keepCommand) {
outgoingText.value = '';
}
}
}
function runCommand(command, requiredEndpoint)
{
console.log("Running command: " + command + " with endpoint: " + requiredEndpoint);
outgoingText.value = command;
if (endpoint !== requiredEndpoint) {
endpoint = requiredEndpoint;
endpointMenu.value = requiredEndpoint;
if (socket && socket.readyState === WebSocket.OPEN) {
console.log("Switching endpoint, disconnecting first");
socket.close();
setTimeout(function() {
outgoingText.value = command;
connectAndSend();
}, 500);
} else {
connectAndSend();
}
} else if (socket && socket.readyState === WebSocket.OPEN) {
sendMessage(true);
} else {
connectAndSend();
}
}
function connectAndSend()
{
server = serverText.value;
endpoint = endpointMenu.value;
serverURL = server + endpoint;
console.log("Connecting to " + serverURL + " to send command");
var commandToSend = outgoingText.value;
var tempSocket = new WebSocket(serverURL);
tempSocket.addEventListener('open', function() {
socket = tempSocket;
openConnection();
setTimeout(function() {
outgoingText.value = commandToSend;
sendMessage(true);
}, 100);
});
tempSocket.addEventListener('close', closeConnection);
tempSocket.addEventListener('message', readReplyMessage);
tempSocket.addEventListener('error', reportError);
}
window.addEventListener('load', setup);