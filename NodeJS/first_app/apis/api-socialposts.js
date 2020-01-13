const date = require('dateformat');
const url = require('url');
const Entities = require('html-entities').AllHtmlEntities;
const entities = new Entities();
let fs = require('fs');
let crypto = require('crypto');

let currentUser;
let authors = new Array();
let posts = new Array();

function passwordHash(word) {
    let mixPassword = crypto.createHash('md5').update(word).digest('hex');
    return mixPassword;
}

function genRandomNumber(digits) {
    let finalNumber = "";
    let numbers = "0123456789";
    for (let i = 0; i < digits + 1; i++) {
        finalNumber += numbers[Math.floor(Math.random()) * (numbers.length - 1)]
    }
    return finalNumber;
}

if (fs.existsSync('users.json')) {
    authors = JSON.parse(fs.readFileSync("users.json"));
}
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
        obj.author = entities.encode(currentUser)
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
    } else if (q.pathname == "/socialposts/register") {
        res.writeHead(200, {
            "Content-type": "application/json"
        });
        let user = {};
        user.authorname = q.query["authorname"];
        let userExists = false;
        for (let usr of authors) {
            if (usr.authorname === user.authorname) {
                userExists = true;
                break;
            }
        }

        if (userExists) {
            user.error = "User already exists";
        } else {
            user.password = passwordHash(q.query["password"]);
            user.email = q.query["email"];
            authors.push(user);
            fs.writeFileSync('users.json', JSON.stringify(authors, null, 2));
        }
    } else if (q.pathname == "/socialposts/login") {
        res.writeHead(200, {
            "Content-type": "application/json"
        });
        let userObj = {};
        userObj.loginName = q.query["authorusername"];
        userObj.loginPassword = passwordHash(q.query["userpassword"]);
        for (let usr of authors) {
            if (usr.authorname == userObj.loginName) {
                currentUser = userObj.loginName;
                break;
            }
        res.end(JSON.stringify(userObj));
        }
    }
}