let numOfCols = prompt("Type the NUMBER of columns for tic-tac-toe: ");
let numOfRows = prompt("Type the NUMBER of rows for tic-tac-toe: ");
let turn = 1; // střídání kol X a O
let gridButtons = document.getElementsByTagName("INPUT");

// funkce na generování hracího pole
function createField() {
    for (let c = 0; c < numOfCols; c++) {
        for (let r = 0; r < numOfRows; r++) {
            let fieldPart = `<input type="button" value=" " class="fieldPart" onclick="checkStatus(this)">`;
            document.getElementById("field").innerHTML += fieldPart;
        }
        document.getElementById("field").innerHTML += `<br>`;
    }
}

// funkce na vytvoření piškvorkové matice
function createGrid() {
    let ticTacToe = [];
    for (let c = 0; c < numOfCols; c++) {
        ticTacToe[c] = []
        for (let r = 0; r < numOfRows; r++) {
            let i = c * numOfRows + r; // gridButtons je seznam numOfCols * numOfRow
            ticTacToe[c][r] = gridButtons[i].value;
        }
    }
    return ticTacToe;
}

// funkce na reset hracího pole
function resetField() {
    document.getElementById("field").innerHTML = null;
    document.getElementById("winmessage").innerHTML = null;
    turn = 1;
    createField();
}

// funkce na kontrolu horizontální výhry
function checkHorizontalWin(tictactoe) {
    for (let c = 0; c < numOfCols; c++) {
        for (let r = 0; r < numOfRows; r++) {
            if (tictactoe[c][r] == "X" && tictactoe[c][r + 1] == "X" && tictactoe[c][r + 2] == "X" && tictactoe[c][r + 3] == "X") {
                // přičítá se index R, protože se to posouvá vodorovně o rows
                document.getElementById("winmessage").innerHTML = `<h1>X is the winner!</h1>`;
                setTimeout(resetField, 1000);
            } else if (tictactoe[c][r] == "O" && tictactoe[c][r + 1] == "O" && tictactoe[c][r + 2] == "O" && tictactoe[c][r + 3] == "O") { 
                document.getElementById("winmessage").innerHTML = `<h1>O is the winner!</h1>`;
                setTimeout(resetField, 1000);
            }
        }
    }
}

// funkce na kontrolu vertikální výhry
function checkVerticalWin(tictactoe) {
    for (let c = 0; c < numOfCols; c++) {
        for (let r = 0; r < numOfRows; r++) {
            if (tictactoe[c][r] == "X" && tictactoe[c + 1][r] == "X" && tictactoe[c + 2][r] == "X" && tictactoe[c + 3][r] == "X") {
                // přičítá se index C, protože se to posouvá vertikálně po columns
                document.getElementById("winmessage").innerHTML = `<h1>X is the winner!</h1>`;
                setTimeout(resetField, 1000);
            } else if (tictactoe[c][r] == "O" && tictactoe[c + 1][r] == "O" && tictactoe[c + 2][r] == "O" && tictactoe[c + 3][r] == "O") { 
                document.getElementById("winmessage").innerHTML = `<h1>O is the winner!</h1>`;
                setTimeout(resetField, 1000);
            }
        }
    }
}

// funkce na diagonální výhru směrem doprava
function checkDiagonalWinRight(tictactoe) {
    for (let c = 0; c < numOfCols; c++) {
        for (let r = 0; r < numOfRows; r++) {
            if (tictactoe[c][r] == "X" && tictactoe[c + 1][r + 1] == "X" && tictactoe[c + 2][r + 2] == "X" && tictactoe[c + 3][r + 3] == "X") {
                // při diagonálním pohybu se pohybuje jak rows tak columns o jedna doprava
                document.getElementById("winmessage").innerHTML = `<h1>X is the winner!</h1>`;
                setTimeout(resetField, 1000);
            } else if (tictactoe[c][r] == "O" && tictactoe[c + 1][r + 1] == "O" && tictactoe[c + 2][r + 2] == "O" && tictactoe[c + 3][r + 3] == "O") { 
                document.getElementById("winmessage").innerHTML = `<h1>O is the winner!</h1>`;
                setTimeout(resetField, 1000);
            }
        }
    }
}

//funkce na diagonální výhru směrem doleva
function checkDiagonalWinLeft(tictactoe) {
    for (let c = 0; c < numOfCols; c++) {
        for (let r = 0; r < numOfRows; r++) {
            if (tictactoe[c][r] == "X" && tictactoe[c + 1][r - 1] == "X" && tictactoe[c + 2][r - 2] == "X" && tictactoe[c + 3][r - 3] == "X") {
                // při diagonálním pohybu se pohybuje columns o jedna dolů, ale rows o jedna delova
                document.getElementById("winmessage").innerHTML = `<h1>X is the winner!</h1>`;
                setTimeout(resetField, 1000);
            } else if (tictactoe[c][r] == "O" && tictactoe[c + 1][r - 1] == "O" && tictactoe[c + 2][r - 2] == "O" && tictactoe[c + 3][r - 3] == "O") { 
                document.getElementById("winmessage").innerHTML = `<h1>O is the winner!</h1>`;
                setTimeout(resetField, 1000);
            }
        }
    }
}

// funkce na změny hodnot X a O, generování matrixu a kontrola výhry
function checkStatus(button) {
    if (turn % 2 !== 0) {
        button.value = "X";
        button.style.color = "blue";
    } else {
        button.value = "O";
        button.style.color = "red";
    }
    button.disabled = "disabled";
    turn += 1;
    checkHorizontalWin(createGrid());
    checkVerticalWin(createGrid());
    checkDiagonalWinRight(createGrid());
    checkDiagonalWinLeft(createGrid());
}