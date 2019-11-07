const DNY_V_TYDNU = ["Neděle","Pondělí","Úterý","Středa","Čtvrtek","Pátek","Sobota"];
const date = require('dateformat');

exports.apiDenVTydnu = function(req, res) {
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
}