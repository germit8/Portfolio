const date = require('dateformat');
const url = require('url');

exports.apiSocialPosts = function(req, res) {
    let q = url.parse(req.url, true);
    res.writeHead(200, {
        "Content-type": "application/json",
        "Access-Control-Allow-Origin":"*"
    });
    let d = new Date();
    let obj = {};

    obj.date = date(d, "dd.mm.yyyy");
    obj.time = d.getHours() + "." + d.getMinutes() + "." + d.getSeconds();
    obj.author = q.query["author"]
    obj.headline = q.query["headline"]
    obj.content = q.query["content"]
    res.end(JSON.stringify(obj));
}