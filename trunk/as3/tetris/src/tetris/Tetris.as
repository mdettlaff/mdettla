package tetris {

    import flash.display.DisplayObjectContainer;
    import flash.display.Sprite;
    import flash.events.IEventDispatcher;
    import flash.events.KeyboardEvent;
    import flash.events.TimerEvent;
    import flash.ui.Keyboard;
    import flash.utils.Timer;

    public class Tetris extends Sprite {

        public static const NEW_GAME_KEY:uint = 78; // N
        public static const PAUSE_KEY:uint = 80; // P

        private static const SPEED:int = 1000;

        private var timer:Timer;

        private var board:Board;
        private var tetrominoCreator:TetrominoCreator;
        private var tetromino:Tetromino;

        public function Tetris(mainContainer:DisplayObjectContainer,
                keyEventDispatcher:IEventDispatcher = null) {
            if (keyEventDispatcher == null) {
                keyEventDispatcher = mainContainer;
            }
            board = new Board();
            tetrominoCreator = new TetrominoCreator();
            putNextTetrominoOnBoard();

            board.addChild(tetromino);
            mainContainer.addChild(board);

            timer = new Timer(SPEED, 0);
            timer.addEventListener(TimerEvent.TIMER, onTimer);
            timer.start();

            keyEventDispatcher.addEventListener(
                    KeyboardEvent.KEY_DOWN, onKeyDown);
            board.addEventListener(
                    TetrisEvent.TETROMINO_LANDED, onLanding);
            board.addEventListener(
                    TetrisEvent.NEW_GAME, onNewGame);
            board.addEventListener(
                    TetrisEvent.GAME_OVER, onGameOver);
            board.addEventListener(
                    TetrisEvent.PAUSE, onPause);
            board.addEventListener(
                    TetrisEvent.CONTINUE, onContinue);
        }

        private function onTimer(event:TimerEvent):void {
            tetromino.moveDown();
        }

        private function onKeyDown(event:KeyboardEvent):void {
            if (timer.running) {
                switch (event.keyCode) {
                    case Keyboard.LEFT:
                        tetromino.moveLeft();
                        break;
                    case Keyboard.RIGHT:
                        tetromino.moveRight();
                        break;
                    case Keyboard.DOWN:
                        tetromino.moveDown();
                        break;
                    case Keyboard.UP:
                        tetromino.rotateClockwise();
                        break;
                }
            }
            switch (event.keyCode) {
                case NEW_GAME_KEY:
                    board.dispatchEvent(new TetrisEvent(TetrisEvent.NEW_GAME));
                    break;
                case PAUSE_KEY:
                    board.dispatchEvent(new TetrisEvent(
                                timer.running
                                ? TetrisEvent.PAUSE
                                : TetrisEvent.CONTINUE));
                    break;
            }
        }

        private function onLanding(event:TetrisEvent):void {
            putNextTetrominoOnBoard();
        }

        private function onNewGame(event:TetrisEvent):void {
            putNextTetrominoOnBoard();
            board.dispatchEvent(new TetrisEvent(TetrisEvent.CONTINUE));
        }

        private function onGameOver(event:TetrisEvent):void {
            timer.stop();
        }

        private function onPause(event:TetrisEvent):void {
            timer.stop();
        }

        private function onContinue(event:TetrisEvent):void {
            timer.start();
        }

        private function putNextTetrominoOnBoard():void {
            tetromino = tetrominoCreator.getNextTetromino();
            if (!board.isConflictWithTetrominoState(
                        tetromino.shape, tetromino.xCoord, tetromino.yCoord)) {
                board.addChild(tetromino);
            } else {
                board.dispatchEvent(new TetrisEvent(TetrisEvent.GAME_OVER));
            }
        }
    }
}
