const http = require('http');
const date = require('dateformat');
const fs = require('fs');
const url = require('url');
const apiDenVTydnu = require('./apis/api-denvtydnu').apiDenVTydnu;
const apiSvatky = require('./apis/api-svatky').apiSvatky;
const apiSocialPosts = require('./apis/api-socialposts').apiSocialPosts;
const createSpaServer = require('spaserver').createSpaServer;

const PORT = 8080;
let citac = 0;

const fileRequested = function(req, res) {
    let q = url.parse(req.url, false);
    let fileName = q.pathname;
    if (fileName == "/") {
        fileName = "/index.html";
    }
    if (fileName.lastIndexOf(".") < 0 || fileName.lastIndexOf(".") < fileName.length - 6) { //pozadavek nema priponu souboru
        return false;
    }
    if (fileName.charAt(0) === '/') {
        fileName = fileName.substr(1);
    }
    if (!fs.existsSync(fileName)) {
        console.error(`File ${fileName} not exists.`);
        res.writeHead(404);
        res.end();
        return true;
    }
}
function processApi(req, res) {

    if (req.pathname == "/socialposts") {
        res.writeHead(302, {
            'Location': '/socialposts.html'
        });
        res.end();
        return;
    }

    if (req.pathname == "/jinastranka") {
        res.writeHead(200, {"Content-type": "text/html"});
        res.end("<html lang='cs'><head><meta charset='UTF8'></head><body>blablabla</body></html>");
    } else if (req.pathname == "/jsoncitac") {
        res.writeHead(200, {"Content-type": "application/json"});
        let obj = {};
        obj.pocetVolani = citac;
        res.end(JSON.stringify(obj));
    } else if (req.pathname == "/denvtydnu") {
        apiDenVTydnu(req, res);
    } else if (req.pathname == "/svatky") {
        apiSvatky(req, res);
    } else if (req.pathname.startsWith("/socialposts/")) {
        apiSocialPosts(req, res);
    } else {
        res.writeHead(200, {"Content-type": "text/html"});
        res.end("<html lang='cs'><head><meta charset='UTF8'></head><body>Počet volání: " +citac + "</body></html>");
    }
}

createSpaServer(PORT, processApi);