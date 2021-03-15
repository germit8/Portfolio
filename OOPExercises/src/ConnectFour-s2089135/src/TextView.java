// import jdk.internal.util.xml.impl.Input;

/**
 * This file is to be completed by you.
 *
 * @author <s2089135>
 */
public final class TextView
{
	public TextView()
	{
	
	}
	
	// ====================================================================================
	// ================================ DISPLAY MESSAGES ==================================
	// ====================================================================================

	public final void displayNewGameMessage() {
		System.out.println("---- GAME STARTED ----");
	}

	public final void displaySaveMessage() {
		System.out.println("TO SAVE GAME ENTER: 0");
	}

	public final void displayGameSavedMessage() {
		System.out.println("Game successfully saved");
	}

	public final void displayLoadMessage() {
		System.out.println("TO LOAD LAST GAME ENTER: -1");
	}

	public final void displayGameLoadedMessage() {
		System.out.println("Game successfully loaded");
	}

	public final void displayLoadedData(Model model) {
		System.out.println("Number of rows: " + model.getNrRows());
		System.out.println("Number of columns: " + model.getNrCols());
		System.out.println("Tokens in line to win: " + model.getConnectN());
		System.out.println("Turn: " + model.getTurn());
		System.out.println("Player 1 is: " + model.getPlayer1());
		System.out.println("Player 2 is: " + model.getPlayer2());
		System.out.println("Player 1 token: " + model.getP1Token());
		System.out.println("Player 2 token: " + model.getP2Token());
		System.out.println("Game mode AI: " + model.getPC());
	}

	public final void displayLoadErrorMessage() {
		System.out.println("Game could not be loaded");
	}

	public final void displayForfeitMessage() {
		System.out.println("TO FORFEIT ENTER: -2");
	}

	public final void displayInvalidMove() {
		System.out.println("This move is invalid!");
	}

	public final void displayGameLoading() {
		System.out.println("Game is loading...");
	}

	public final void displayThankYou() {
		System.out.println("Thank you for playing, see you later!");
	}

	public final void displayDrawMessage() {
		System.out.println("You have filled the board - it is a draw!");
	}

	public final void displayWin(String name) {
		System.out.println(name + " is the winner!");
	}

	public final void displayTurn(String name) {
		System.out.println("It is " + name + "'s turn");
	}
	
	// ====================================================================================
	// ================================== ASK MESSAGES ====================================
	// ====================================================================================

	public final int askForMove() {
		System.out.print("Select a free column: ");
		return InputUtil.readIntFromUser();
	}

	public final char askForLoad() {
		System.out.print("Would you like to load game? Press 'y' to confirm, anything else to decline: ");
		return InputUtil.readCharFromUser();
	}

	public final char askForPCMode() {
		System.out.print("Would you like to play against a computer? Press 'y' to confirm, anything else to decline: ");
		return InputUtil.readCharFromUser();
	}

	public final char askForNewGame() {
		System.out.print("Would you like to play again? Press 'y' to confirm, anything else to decline: ");
		return InputUtil.readCharFromUser();
	}

	public final char askForSettings() {
		System.out.print("Would you like to customise settings? Press 'y' to confirm, anything else to decline: ");
		return InputUtil.readCharFromUser();
	}

	public final String askForName() {
		System.out.print("Hello, how would you like to be called? ");
		return InputUtil.readStringFromUser();
	}

	public final String askForName2() {
		System.out.print("How would you like to be called Player 2? ");
		return InputUtil.readStringFromUser();
	}

	public final String askForP1Token(String name) {
		System.out.print(name + " choose a token (any character except capital O): ");
		String pToken = InputUtil.readStringFromUser();
		while (pToken.equals("O") || pToken.length() > 1) {
			System.out.println("Please choose a valid token: ");
			pToken = InputUtil.readStringFromUser();
		}
		return pToken;
	}

	public final String askForP2Token(String name, String p1Token) {
		System.out.print(name + " choose a token (any character except capital O): ");
		String pToken = InputUtil.readStringFromUser();
		while (pToken.equals("O") || pToken.length() > 1 || pToken.equals(p1Token)) {
			System.out.println("Please choose a valid token: ");
			pToken = InputUtil.readStringFromUser();
		}
		return pToken;
	}

	public final String askForAIToken(String p1Token) {
		System.out.print("Choose a token for AI (any character except capital O): ");
		String pToken = InputUtil.readStringFromUser();
		while (pToken.equals("O") || pToken.length() > 1 || pToken.equals(p1Token)) {
			System.out.println("Please choose a valid token, try again: ");
			pToken = InputUtil.readStringFromUser();
		}
		return pToken;
	}

	public final int askForRows() {
		System.out.print("Set rows to: ");
		int userInput = InputUtil.readIntFromUser();
		while (userInput <= 0) {
			System.out.println("Please choose a positive integer: ");
			userInput = InputUtil.readIntFromUser();
		}
		return userInput;
	}

	public final int askForCols() {
		System.out.print("Set cols to: ");
		int userInput = InputUtil.readIntFromUser();
		while (userInput <= 0) {
			System.out.println("Please choose a positive integer: ");
			userInput = InputUtil.readIntFromUser();
		}
		return userInput;
	}

	public final int askForConnectN(int nrRows, int nrCols) {
		System.out.print("How many tokens in row to win: ");
		int userInput = InputUtil.readIntFromUser();
		while (userInput > Math.max(nrCols, nrRows) || userInput < 1) {
			System.out.println("Please choose a meaningful number of tokens: ");
			userInput = InputUtil.readIntFromUser();
		}
		return userInput;
	}
	
	public final void displayBoard(Model model)
	{
		// Board representation variables
		int nrRows = model.getNrRows();
		int nrCols = model.getNrCols();
		String[][] grid = model.getGrid();
		
		String rowDivider = "-".repeat(2 * nrCols-1);
		
		// A StringBuilder is used to assemble longer Strings more efficiently.
		StringBuilder sb = new StringBuilder();
		sb.append(rowDivider);
		sb.append("\n");

		// loop for printing out the board representation
		for (int r = 0; r < nrRows; r++) {
			for (int c = 0; c < nrCols; c++) {
				sb.append(grid[r][c] + " ");
			}
			sb.append("\n");
		}
		sb.append(rowDivider);
		
		System.out.println(sb);
	}
}
