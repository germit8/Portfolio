let headOne = document.querySelector("h1");
let navFacebook = document.querySelector(".nav-item");

function bamboozleMesssage() {
    headOne.textContent = "NOT! Sike, you thought!";
}

function backToNormalMessage() {
    headOne.textContent = "I wish you a great day... ♥";
}

function sadMessage() {
    navFacebook.textContent = "Why would you use facebook... :(";
}

headOne.addEventListener("mouseover", bamboozleMesssage)

headOne.addEventListener("mouseout", backToNormalMessage)

navFacebook.addEventListener("click", sadMessage)