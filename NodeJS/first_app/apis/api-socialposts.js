const date = require('dateformat');
const url = require('url');
const Entities = require('html-entities').AllHtmlEntities;
const entities = new Entities();

let posts = new Array();
exports.apiSocialPosts = function(req, res) {
    let q = url.parse(req.url, true);
    if (q.pathname == "/socialposts/listposts") {
        res.writeHead(200, {
            "Content-type": "application/json"
        });
        let obj = {};
        obj.allPosts = posts;
        res.end(JSON.stringify(obj));
    } else if (q.pathname == "/socialposts/addpost") {
        res.writeHead(200, {
            "Content-type": "application/json"
        });
        let d = new Date();
        let obj = {};
    
        obj.date = date(d, "dd.mm.yyyy");
        obj.time = d.getHours() + "." + d.getMinutes() + "." + d.getSeconds();
        obj.author = entities.encode(q.query["author"])
        obj.headline = entities.encode(q.query["headline"])
        obj.content = entities.encode(q.query["content"])
        obj.imageurl = entities.encode(q.query["imageurl"])
        obj.slug = ((obj.author.concat(obj.headline)).replace(/\s+/g, '').toLowerCase()).substring(0, 10);
        for (pst of posts) {
            if (obj.slug == pst.slug) {
                obj.slug += genRandomNumber(4);
            }
        }
        posts.push(obj);
        res.end(JSON.stringify(obj));
    } else if (q.pathname == "/socialposts/deletepost") {
        res.writeHead(200, {
            "Content-type": "application/json"
        });
        let obj = {};
        let objSlug = q.query["slug"]
        obj.allPosts = posts;
        for (let pst of obj.allPosts) {
            if (pst.slug == objSlug) {
                obj.allPosts.splice(obj.allPosts.indexOf(pst), 1);
            }
        }
        res.end(JSON.stringify(obj));
    }
}

function genRandomNumber(digits) {
    let finalNumber = "";
    let numbers = "0123456789";
    for (let i = 0; i < digits + 1; i++) {
        finalNumber += numbers[Math.floor(Math.random()) * (numbers.length - 1)]
    }
    return finalNumber;
}