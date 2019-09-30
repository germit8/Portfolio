const http = require('http');
const date = require('dateformat');
const DNY_V_TYDNU = ["Neděle","Pondělí","Úterý","Středa","Čtvrtek","Pátek","Sobota"];

let citac = 0;

http.createServer((req, res) => {
    if (req.url == "/") {
        citac++; //dtto citac=citac+1
    }
    if (req.url == "/jinastranka") {
        res.writeHead(200, {"Content-type": "text/html"});
        res.end("<html lang='cs'><head><meta charset='UTF8'></head><body>blablabla</body></html>");
    } else if (req.url == "/jsondemo") {
        res.writeHead(200, {"Content-type": "application/json"});
        let obj = {};
        obj.jmeno = "Bob";
        obj.prijmeni = "Bobíček";
        obj.rokNarozeni = 2002;
        res.end(JSON.stringify(obj));
    } else if (req.url == "/jsoncitac") {
        res.writeHead(200, {"Content-type": "application/json"});
        let obj = {};
        obj.pocetVolani = citac;
        res.end(JSON.stringify(obj));
    } else if (req.url == "/denvtydnu") {
        res.writeHead(200, {
            "Content-type": "application/json",
            "Access-Control-Allow-Origin":"*"
        });
        let d = new Date();
        let obj = {};
        obj.systDatum = d;
        obj.denVTydnuCiselne = d.getDay(); //0...nedele, 1...pondeli,...
        obj.datumCesky = date(d, "dd.mm.yyyy");
        obj.casCesky = d.getHours() + "." + d.getMinutes() + "." + d.getSeconds();
        obj.denVTydnuCesky = DNY_V_TYDNU[d.getDay()];
        res.end(JSON.stringify(obj));
    } else {
        res.writeHead(200, {"Content-type": "text/html"});
        res.end("<html lang='cs'><head><meta charset='UTF8'></head><body>Počet volání: " +citac + "</body></html>");
    }
}).listen(8888);