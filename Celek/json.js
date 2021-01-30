// FUNKCE NA FRONTEND
// datum a svátek
function nameDay() {
    let response = getHttpResponse("http://nodejs-3260.rostiapp.cz/date/");
    let object = JSON.parse(response);
    let lowerDay = object.dow.toLowerCase();
    let output = "Today is " + lowerDay + " " + object.date.bold() + " and nameday has " + object.svatek.bold();
    document.getElementById("output").innerHTML = output;
  }

// čas
function clockTime() {
    let response = getHttpResponse("http://nodejs-3260.rostiapp.cz/date/");
    let object = JSON.parse(response);
    let output = "Time: " + object.time.bold();
    document.getElementById("output2").innerHTML = output;
}

function timeIncrement() {
    setInterval(clockTime, 1000);
}

function createRandomColor() {
    let letters = "0123456789ABCDEF";
    let randomcolor = "#";
    for (let i = 0; i < 6; i++) {
        randomcolor += letters[Math.floor(Math.random() * 16)];
    }
    return randomcolor
}

function colorChange() {
    let heading = document.querySelector("h1");
    // let timer = document.querySelector("#output2");
    // let info = document.querySelector("#output");
    // timer.style.color = createRandomColor();
    heading.style.color = createRandomColor();
    // info.style.color = createRandomColor();
}

function colorTime() {
    setInterval(colorChange, 250);
}

function logout() {
    document.querySelector("form").style.display = "none";
}



// FUNKCE NA CHAT
let globalBodyObject = {}

function goToLogin() {
    document.getElementById("useremail").style.display = "none";
    document.getElementById("userconfirmpassword").style.display = "none";
    document.getElementById("register").style.display = "none";
    document.getElementById("gotologin").style.display = "none";
    document.getElementById("login").style.display = "inline";
    document.getElementById("gotoregister").style.display = "inline";
    document.getElementById("userfullname").style.display = "none";
    document.getElementById("pass").style.display = "none";
    document.getElementById("mail").style.display = "none";
}

function goToRegister() {
    document.getElementById("useremail").style.display = "";
    document.getElementById("userconfirmpassword").style.display = "";
    document.getElementById("register").style.display = "";
    document.getElementById("gotologin").style.display = "";
    document.getElementById("login").style.display = "none";
    document.getElementById("gotoregister").style.display = "none";
    document.getElementById("userfullname").style.display = "";
    document.getElementById("divUsername").style.display = "";
    document.getElementById("divPassword").style.display = "";
    document.getElementById("username").value = "";
    document.getElementById("password").value = "";
    document.getElementById("messenger").style.display = "";
}

function checkPassword() {
    let passwrd = document.getElementById("password").value;
    let confpasswrd = document.getElementById("confirmpassword").value;
    if (passwrd !== confpasswrd) {
        document.querySelector("#pass").innerHTML = "Passwords don't match! Try again!";
    } else {
        globalBodyObject.password = document.getElementById("password").value;
        document.querySelector("#pass").innerHTML = "";
    }
}

function checkEmail() {
    let mail = document.getElementById("email").value;
    let n = mail.includes("@");
    if (n == false) {
        document.querySelector("#mail").innerHTML = "An email must contain @";
    } else {
        globalBodyObject.email = document.getElementById("email").value;
        document.querySelector("#mail").innerHTML = "";
    }
}

function register() {
    let url = "https://nodejs-3260.rostiapp.cz/users/registry";

    checkPassword()
    checkEmail()

    globalBodyObject.username = document.getElementById("username").value;
    globalBodyObject.fullname = document.getElementById("fullname").value;

    let body = JSON.stringify(globalBodyObject);
    let s = postHttpRequest(url, body);
    let object = JSON.parse(s);

    if (object.status === "OK") {
        alert("You have successfully registered!")
    } else {
        alert(object.error);
    }    
}

function login() {
    let url = "https://nodejs-3260.rostiapp.cz/users/login";
    let password = document.getElementById("password").value;
    let username = document.getElementById("username").value;

    let bodyObjct = {}

    bodyObjct.username = username;
    bodyObjct.password = password;

    let body = JSON.stringify(bodyObjct);
    let r = postHttpRequest(url, body);
    let object = JSON.parse(r);

    if (object.token) {
        token = object.token;
        alert("You have logged in!")
        document.getElementById("divUsername").style.display = "none";
        document.getElementById("divPassword").style.display = "none";
        document.getElementById("login").style.display = "none";
        document.getElementById("gotoregister").style.display = "none";
        document.getElementById("messenger").style.display = "block";
    } else {
        alert(object.error)
    }
     
}

function logout() {
    let url = "https://nodejs-3260.rostiapp.cz/users/logout";

    if (getHttpResponse(url, token)) {
        alert("You have logged out!")
        goToRegister();
    } else {
        alert("Error!")
    }
}

function addMessage() {
    let urlAddMessage = "https://nodejs-3260.rostiapp.cz/chat/addMsg";
    let bodyObject = {};

    bodyObject.chat = "o4qfmd0tv44571si1orqats9";
    bodyObject.usr = token;
    bodyObject.msg = document.getElementById("usermessage").value;

    let body = JSON.stringify(bodyObject);
    let t = postHttpRequest(urlAddMessage, body);
    let obj = JSON.parse(t);
    console.log(obj)

    return obj
}

function showMessages() {
    let messagesObject = addMessage();
    let messages = "";

    for (let i = 0; i < messagesObject.length; i++) {
        messages += messagesObject[i].msg + "\n";
    }
    document.getElementById("userchat").textContent = messages;
}

// Zkusit vytvořit místo textarea obyčejný div s ohraničením, kam budu vkládat zprávy ve formě paragrafů