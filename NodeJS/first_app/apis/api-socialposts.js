const date = require('dateformat');
const url = require('url');

let posts = new Array();
exports.apiSocialPosts = function(req, res) {
    let q = url.parse(req.url, true);
    if (q.pathname == "/socialposts/listposts") {
        res.writeHead(200, {
            "Content-type": "application/json",
            "Access-Control-Allow-Origin":"*"
        });
        let obj = {};
        obj.allPosts = posts;
        res.end(JSON.stringify(obj));
    } else if (q.pathname == "/socialposts/addpost") {
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
        obj.imageurl = q.query["imageurl"]
        posts.push(obj);
        res.end(JSON.stringify(obj));
    } else if (q.pathname == "/socialposts/deletepost") {
        res.writeHead(200, {
            "Content-type": "application/json",
            "Access-Control-Allow-Origin":"*"
        });
        let obj = {};
        obj.allPosts = posts;
        obj.allPosts.splice(obj.allPosts.indexOf(this), 1);
        res.end(JSON.stringify(obj));
    }
}