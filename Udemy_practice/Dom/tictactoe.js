let restart = document.querySelector("#restart");
let squares = document.querySelectorAll("td");

function restartGame() {
    for (let i = 0; i < squares.length; i++) {
        squares[i].textContent = "";
    }
}

restart.addEventListener("click", restartGame)

function changeMarker() {
    if (this.textContent === "") {
        this.textContent = "X";
    } else if (this.textContent === "X") {
        this.textContent = "O";
    } else {
        this.textContent = "";
    }
}

for (let j = 0; j < squares.length; j++) {
    squares[j].addEventListener("click", changeMarker);
}