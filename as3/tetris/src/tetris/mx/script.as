import tetris.Board;
import tetris.Tetris;
import tetris.TetrisEvent;

import flash.events.Event;
import mx.controls.Alert;
import mx.events.FlexEvent;
import mx.rpc.events.ResultEvent;

private static const PAUSE:String = "Pauza";
private static const CONTINUE:String = "Wznów";

[Bindable]
private var score:int = 0;

private function init():void {
    new Tetris(gameCanvas, this);
    gameCanvas.width = Board.WIDTH * Board.BLOCK_SIZE;
    gameCanvas.height = Board.HEIGHT * Board.BLOCK_SIZE;

    newGameButton.addEventListener(
            FlexEvent.BUTTON_DOWN, onNewGameButtonClicked);
    pauseButton.addEventListener(
            FlexEvent.BUTTON_DOWN, onPauseButtonClicked);

    addEventListener(TetrisEvent.LINES_DESTROYED, awardPoints);
    addEventListener(TetrisEvent.NEW_GAME, onNewGame);
    addEventListener(TetrisEvent.GAME_OVER, onGameOver);
    addEventListener(TetrisEvent.PAUSE, onPause);
    addEventListener(TetrisEvent.CONTINUE, onContinue);
}

private function onNewGameButtonClicked(event:Event):void {
    dispatchTetrisEvent(new TetrisEvent(TetrisEvent.NEW_GAME));
}

private function onPauseButtonClicked(event:Event):void {
    if (event.currentTarget.label == PAUSE) {
        dispatchTetrisEvent(new TetrisEvent(TetrisEvent.PAUSE));
    } else {
        dispatchTetrisEvent(new TetrisEvent(TetrisEvent.CONTINUE));
    }
}

private function awardPoints(event:TetrisEvent):void {
    score += 100 * event.destroyedLinesCount;
}

private function onNewGame(event:TetrisEvent):void {
    score = 0;
    pauseButton.enabled = true;
}

private function onGameOver(event:TetrisEvent):void {
    pauseButton.enabled = false;
    if (score > 0) {
        submitScore();
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

private function dispatchTetrisEvent(event:TetrisEvent):void {
    gameCanvas.getChildAt(0).dispatchEvent(event);
}

private function submitScore():void {
    submitScoreService.cancel();
    var params:Object = new Object();
    params.score = score;
    submitScoreService.send(params);
}
