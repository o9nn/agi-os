var LinkGrammar = require(__dirname + "/../build/index");
var linkGrammar = new LinkGrammar();
var linkage = linkGrammar.parse('turn off the light');
var mvLinks = linkage.linksByLabel('MV');
var connections = linkage.getConnectorWords('off');