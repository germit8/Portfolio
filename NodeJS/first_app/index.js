const http = require('http');
const date = require('dateformat');
const fs = require('fs');
const url = require('url');
const apiDenVTydnu = require('./apis/api-denvtydnu').apiDenVTydnu;
const apiSvatky = require('./apis/api-svatky').apiSvatky;
const apiChat = require('./apis/api-chat').apiChat;
const apiSocialPosts = require('./apis/api-socialposts').apiSocialPosts;

let citac = 0;

function processStaticFiles(res, fileName) {
    fileName = fileName.substr(1)
    let contentType = "text/html";
    if (fileName.endsWith(".jpeg")) {
        contentType = "image/jpeg"
    }
    if (fs.existsSync(fileName)) {
        fs.readFile(fileName, function(err, data) {
            res.writeHead(200, {'Content-Type': contentType});
            res.write(data);
            res.end();
        });
    } else {
        res.writeHead(404);
        res.end();
    }
}
http.createServer((req, res) => {
    let q = url.parse(req.url, true);

    if (q.pathname == "/") {
        citac++; //dtto citac=citac+1
        processStaticFiles(res, "/index.html")
        return;
    }
    if (q.pathname == "/chat") {
        processStaticFiles(res, "/chat.html")
        return;
    }
    if (q.pathname == "/socialposts") {
        processStaticFiles(res, "/socialposts.html")
        return;
    }

    if (q.pathname.length - q.pathname.lastIndexOf(".") < 6) {
        processStaticFiles(res, q.pathname)
        return;
    }

    if (q.pathname == "/jinastranka") {
        res.writeHead(200, {"Content-type": "text/html"});
        res.end("<html lang='cs'><head><meta charset='UTF8'></head><body>blablabla</body></html>");
    } else if (q.pathname == "/jsoncitac") {
        res.writeHead(200, {"Content-type": "application/json"});
        let obj = {};
        obj.pocetVolani = citac;
        res.end(JSON.stringify(obj));
    } else if (q.pathname == "/denvtydnu") {
        apiDenVTydnu(req, res);
    } else if (q.pathname == "/svatky") {
        apiSvatky(req, res);
    } else if (q.pathname.startsWith("/socialposts/")) {
        apiSocialPosts(req, res);
    } else if (q.pathname.startsWith("/chat/")) {
        apiChat(req, res);
    } else {
        res.writeHead(200, {"Content-type": "text/html"});
        res.end("<html lang='cs'><head><meta charset='UTF8'></head><body>Počet volání: " +citac + "</body></html>");
    }
}).listen(8888);