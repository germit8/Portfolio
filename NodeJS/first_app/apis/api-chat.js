const url = require('url');
let messages = new Array();

exports.apiChat = function(req, res) {
    let q = url.parse(req.url, true);
    res.writeHead(200, {"Content-type": "application/json"});
    let obj = {};
    
    if (q.pathname == "/chat/add") {
        obj.text = q.query["msg"];
        obj.time = new Date();
        messages.push(obj);
    } else {
        obj.allMessages = messages;
    }
    res.end(JSON.stringify(obj));
}