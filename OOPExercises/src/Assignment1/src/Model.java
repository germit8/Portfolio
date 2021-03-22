import java.io.File;
import java.io.IOException;
import java.io.FileWriter;
import java.io.FileNotFoundException;
import java.util.Scanner;
/**
 * This file is to be completed by you.
 *
 * @author <s2089135>
 */
public final class Model {
	// ===========================================================================
	// ================================ CONSTANTS ================================
	// ===========================================================================
	// The most common version of Connect Four has 6 rows and 7 columns.
	public static final transient int DEFAULT_NR_ROWS = 6;
	public static final transient int DEFAULT_NR_COLS = 7;
	
	// ========================================================================
	// ================================ FIELDS ================================
	// ========================================================================
	// The size and 2D representation of the board
	private int nrRows;
	private int nrCols;
	private String[][] myGrid;

	// number of tokens in row to win, turn counter and win boolean
	private int connectN;
	private int turn;
	private boolean win;

	// player + token representations, AI mode
	private String player1;
	private String player2;
	private String p1token;
	private String p2token;
	private boolean pcMode;
	// =============================================================================
	// ================================ CONSTRUCTOR ================================
	// =============================================================================
	public Model() {
		// Initialise everything to default (used if the option to change settings is declined)
		nrRows = DEFAULT_NR_ROWS;
		nrCols = DEFAULT_NR_COLS;
		myGrid = createGrid();

		connectN = 4;
		turn = 1;
		win = false;

		player1 = "Player 1";
		player2 = "Player 2";
		p1token = "1";
		p2token = "2";
		pcMode = false;
	}
	
	// ====================================================================================
	// ================================ MODEL INTERACTIONS ================================
	// ====================================================================================
	
	// function that resets the game
	public void resetGame() {
		myGrid = createGrid();
		turn = 1;
		win = false;
	}

	// uses "O" to represent empty spaces in the board based on currently set nrCols+Rows
	public String[][] createGrid() {
		String[][] grid = new String[nrRows][nrCols];
		for (int c = 0; c < nrCols; c++) {
			for (int r = 0; r < nrRows; r++) {
				grid[r][c] = "O";
			}
		}
		return grid;
	}

	// First checks if input is in range of the columns and then if the column is not full
	public boolean isMoveValid(int move) {
		if (move <= nrCols && move > 0) {
			if (myGrid[0][move-1].equals("O")) {
				return true;
			} else {
				return false; 
			} 
		} else {
			return false;
		}
	}
	
	// the column with index move stays the same and the loop goes through the rows from the back
	public void makeMove(int move) {
		for (int r = nrRows - 1; r >= 0; r--) {
			if (myGrid[r][move-1].equals("O")) {
				if (turn % 2 != 0) {
					myGrid[r][move-1] = p1token;
					break;
				} else {
					myGrid[r][move-1] = p2token;
					break;
				}
			}
		}
		win = checkWin();
	}

	// AI takes random but valid integer a tries to drop it
	public boolean computerMakeMove() {
		boolean success = false;
		int move = 1 + (int) (Math.random() * nrCols); // range [1, nrCols]

		for (int r = nrRows - 1; r >= 0; r--) {
			if (myGrid[r][move-1].equals("O")) {
				myGrid[r][move-1] = p2token;
				success = true;
				break;
			} else {
				// if column is full, function returns false and so it keeps on trying until it succeeds
				success = false;
			}
		}
		win = checkWin();
		return success;
	}

	// Checks all 4 types of winning - on a row, column and both diagonals
	public boolean checkWin() {
		// counter for how many same tokens are next to each other - default is 1
		int tokensInRow = 1;
		/** HORIZONTAL WIN
		 * Goes through all rows, but only through number of columns minus winning number of tokens minus one
		*/
		for (int r = 0; r < nrRows; r++) {
			for (int c = 0; c < nrCols-(connectN-1); c++) {
				if (!(myGrid[r][c].equals("O"))) {
					for (int k = 1; k < connectN; k++) {
						if (myGrid[r][c].equals(myGrid[r][c+k])) {
							tokensInRow++;
						}
					}
					if (tokensInRow == connectN) {
						return true;
					}
					tokensInRow = 1;
				}
			}
		}
		// VERTICAL WIN
		for (int r = 0; r < nrRows-(connectN-1); r++) {
			for (int c = 0; c < nrCols; c++) {
				if (!(myGrid[r][c].equals("O"))) {
					for (int k = 1; k < connectN; k++) {
						if (myGrid[r][c].equals(myGrid[r+k][c])) {
							tokensInRow++;
						}
					}
					if (tokensInRow == connectN) {
						return true;
					}
					tokensInRow = 1;
				}
			}
		}
		/** DIAGONAL WIN (from left-down to right-up)
		 * There is only a certain segment (diagram in report) in the 2D array where the starting position of token
		 * can be so that it is possible to get the desired winning number of tokens in diagonal
		 */
		for (int r = nrRows-1; r > connectN-2; r--) {
			for (int c = 0; c < nrCols-(connectN-1); c++) {
				if (!(myGrid[r][c].equals("O"))) {
					for (int k = 1; k < connectN; k++) {
						if (myGrid[r][c].equals(myGrid[r-k][c+k])) {
							tokensInRow++;
						}
					}
					if (tokensInRow == connectN) {
						return true;
					}
					tokensInRow = 1;
				}
			}
		}
		// DIAGONAL WIN (from left-up to right-down)
		for (int r = 0; r < nrRows-(connectN-1); r++) {
			for (int c = 0; c < nrCols-(connectN-1); c++) {
				if (!(myGrid[r][c].equals("O"))) {
					for (int k = 1; k < connectN; k++) {
						if (myGrid[r][c].equals(myGrid[r+k][c+k])) {
							tokensInRow++;
						}
					}
					if (tokensInRow == connectN) {
						return true;
					}
					tokensInRow = 1;
				}
			}
		}
		return false;
	}

	// Function that creates a new save file if there is not one
	public void createSaveFile() {
		try {
			File myFile = new File("gameState.txt");
			myFile.createNewFile();
		} catch (IOException err) {
			err.printStackTrace();
		}
	}

	// Function that saves game into the created or already existing file
	// all of these parameters are represented on new lines
	public boolean saveGame() {
		try {
			FileWriter myWriter = new FileWriter("gameState.txt");

			myWriter.write("" + nrRows + "\n");
			myWriter.write("" + nrCols + "\n");
			myWriter.write("" + connectN + "\n");
			myWriter.write("" + turn + "\n");
			myWriter.write(player1 + "\n");
			myWriter.write(player2 + "\n");
			myWriter.write(p1token + "\n");
			myWriter.write(p2token + "\n");
			myWriter.write("" + pcMode + "\n");

			// tokens from game board are appended to one long String
			for (int r = 0; r < nrRows; r++) {
				for (int c = 0; c < nrCols; c++) {
					myWriter.write(myGrid[r][c]);
				}
			}

			myWriter.close();
			return true;
		} catch (IOException err) {
			err.printStackTrace();
			return false;
		}
	}

	// Function that loads all the data from the save file
	public boolean loadGame() {
		try {
			File myFile = new File("gameState.txt");
			Scanner myReader = new Scanner(myFile);

			// Data is loaded in the same order as it is saved
			nrRows = Integer.parseInt(myReader.nextLine());
			nrCols = Integer.parseInt(myReader.nextLine());
			connectN = Integer.parseInt(myReader.nextLine());
			turn = Integer.parseInt(myReader.nextLine());
			player1 = myReader.nextLine();
			player2 = myReader.nextLine();
			p1token = myReader.nextLine();
			p2token = myReader.nextLine();
			pcMode = Boolean.parseBoolean(myReader.nextLine());

			// theGrid represents the long String of board tokens
			String theGrid = myReader.nextLine();
			
			// Before filling the board with tokens, we have to contstruct the new board with the
			// parametres loaded from save file (nrRows, nrCols), otherwise the base board is 6x7 and
			// we would get ArrayIndexOutOfBounds error when trying to acces for example 8th row/column
			myGrid = createGrid();
			for (int r = 0; r < nrRows; r++) {
				for (int c = 0; c < nrCols; c++) {
					myGrid[r][c] = "" + theGrid.charAt(r*nrCols+c);
				}
			}

			myReader.close();
			return true;
		} catch (FileNotFoundException e) {
			// e.printStackTrace();
			return false;
		}
	}

	// =========================================================================
	// ================================ SETTERS ================================
	// =========================================================================
	public void changeTurn(int num) {
		this.turn += num;
	}

	public void setRows(int rows) {
		this.nrRows = rows;
	}

	public void setCols(int cols) {
		this.nrCols = cols;
	}

	public void setGrid() {
		this.myGrid = createGrid();
	}

	public void setConnectN(int input) {
		this.connectN = input;
	}

	public void setToken1(String choice) {
		this.p1token = choice;
	}

	public void setToken2(String choice) {
		this.p2token = choice;
	}

	public void setPC(boolean mode) {
		this.pcMode = mode;
	}

	public void setPlayer1(String name) {
		this.player1 = name;
	}
	
	public void setPlayer2(String name) {
		this.player2 = name;
	}

	public void setWin(boolean win) {
		this.win = win;
	}
	// =========================================================================
	// ================================ GETTERS ================================
	// =========================================================================
	public int getNrRows() {
		return nrRows;
	}
	
	public int getNrCols() {
		return nrCols;
	}

	public String[][] getGrid() {
		return myGrid;
	}

	public int getTurn() {
		return turn;
	}

	public boolean getWin() {
		return win;
	}

	public boolean getPC() {
		return pcMode;
	}

	public String getPlayer1() {
		return player1;
	}

	public String getPlayer2() {
		return player2;
	}

	public int getConnectN() {
		return connectN;
	}

	public String getP1Token() {
		return p1token;
	}

	public String getP2Token() {
		return p2token;
	}
}
