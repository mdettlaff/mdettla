import tetris.Board;
import tetris.Tetris;
import tetris.TetrisEvent;
import tetris.Tetromino;
import tetris.Utils;

import flash.events.Event;
import mx.controls.Alert;
import mx.events.FlexEvent;
import mx.rpc.events.ResultEvent;

private static const PAUSE:String = "Pauza";
private static const CONTINUE:String = "Wznów";

[Bindable]
private var score:int = 0;

private function init():void {
    initEventListeners();
    initUIComponents();

    new Tetris(gameArea, this);
}

private function initEventListeners():void {
    newGameButton.addEventListener(
            FlexEvent.BUTTON_DOWN, onNewGameButtonClicked);
    pauseButton.addEventListener(
            FlexEvent.BUTTON_DOWN, onPauseButtonClicked);

    addEventListener(TetrisEvent.TETROMINO_LANDED, showNextTetromino);
    addEventListener(TetrisEvent.LINES_DESTROYED, awardPoints);
    addEventListener(TetrisEvent.NEW_GAME, onNewGame);
    addEventListener(TetrisEvent.GAME_OVER, onGameOver);
    addEventListener(TetrisEvent.PAUSE, onPause);
    addEventListener(TetrisEvent.CONTINUE, onContinue);
}

private function initUIComponents():void {
    gameArea.width = Board.WIDTH * Board.BLOCK_SIZE;
    gameArea.height = Board.HEIGHT * Board.BLOCK_SIZE;
    nextTetrominoCanvas.width = 4 * Board.BLOCK_SIZE;
    gameInfoCanvas.height = 2 * Board.BLOCK_SIZE + 12;
}

private function onNewGameButtonClicked(event:Event):void {
    gameArea.dispatchEvent(new TetrisEvent(TetrisEvent.NEW_GAME));
    callLater(gameArea.setFocus);
}

private function onPauseButtonClicked(event:Event):void {
    if (event.currentTarget.label == PAUSE) {
        gameArea.dispatchEvent(new TetrisEvent(TetrisEvent.PAUSE));
    } else {
        gameArea.dispatchEvent(new TetrisEvent(TetrisEvent.CONTINUE));
    }
    callLater(gameArea.setFocus);
}

private function awardPoints(event:TetrisEvent):void {
    score += 100 * event.destroyedLinesCount;
}

private function showNextTetromino(event:TetrisEvent):void {
    Utils.removeAllChildren(nextTetrominoCanvas);
    event.nextTetromino.x = 0;
    event.nextTetromino.y = 0;
    nextTetrominoCanvas.addChild(event.nextTetromino);
}

private function onNewGame(event:TetrisEvent):void {
    score = 0;
    pauseButton.enabled = true;
    showNextTetromino(event);
}

private function onGameOver(event:TetrisEvent):void {
    pauseButton.enabled = false;
    if (score > 0) {
        submitScore(score);
    }
}

private function onPause(event:TetrisEvent):void {
    pauseButton.label = CONTINUE;
}

private function onContinue(event:TetrisEvent):void {
    pauseButton.label = PAUSE;
}

private function onScoreServiceResult(event:ResultEvent):void {
    var serviceResponse:String = event.result.toString();
    if ("OK" != serviceResponse) {
        Alert.show("Wynik nie został zapisany.\n" + serviceResponse);
    }
}

private function submitScore(score:int):void {
    submitScoreService.cancel();
    var params:Object = new Object();
    params.score = score;
    submitScoreService.send(params);
}
