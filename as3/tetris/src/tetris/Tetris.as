package tetris {

    import flash.display.DisplayObjectContainer;
    import flash.display.Sprite;
    import flash.events.KeyboardEvent;
    import flash.events.TimerEvent;
    import flash.ui.Keyboard;
    import flash.utils.Timer;

    public class Tetris extends Sprite {

        private static const SPEED:int = 1000;

        private var timer:Timer;

        private var board:Board;
        private var tetrominoCreator:TetrominoCreator;
        private var tetromino:Tetromino;

        public function Tetris(mainContainer:DisplayObjectContainer) {
            board = new Board();
            tetrominoCreator = new TetrominoCreator();
            putNextTetrominoOnBoard();

            board.addChild(tetromino);
            mainContainer.addChild(board);

            timer = new Timer(SPEED, 0);
            timer.addEventListener(TimerEvent.TIMER, timerHandler);
            timer.start();

            mainContainer.addEventListener(KeyboardEvent.KEY_DOWN, keyHandler);
            board.addEventListener(
                    TetrisEvent.TETROMINO_LANDED, landingHandler);
            board.addEventListener(
                    TetrisEvent.NEW_GAME, newGameHandler);
            board.addEventListener(
                    TetrisEvent.PAUSE, pauseHandler);
            board.addEventListener(
                    TetrisEvent.CONTINUE, continueHandler);
        }

        private function timerHandler(event:TimerEvent):void {
            tetromino.moveDown();
        }

        private function keyHandler(event:KeyboardEvent):void {
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
                case 78: // N
                    board.dispatchEvent(new TetrisEvent(TetrisEvent.NEW_GAME));
                    break;
                case 80: // P
                    board.dispatchEvent(new TetrisEvent(
                                timer.running
                                ? TetrisEvent.PAUSE
                                : TetrisEvent.CONTINUE));
                    break;
            }
        }

        private function landingHandler(event:TetrisEvent):void {
            board.removeChild(tetromino);
            putNextTetrominoOnBoard();
        }

        private function newGameHandler(event:TetrisEvent):void {
            board.removeChild(tetromino);
            putNextTetrominoOnBoard();
            board.dispatchEvent(new TetrisEvent(TetrisEvent.CONTINUE));
        }

        private function pauseHandler(event:TetrisEvent):void {
            timer.stop();
        }

        private function continueHandler(event:TetrisEvent):void {
            timer.start();
        }

        private function putNextTetrominoOnBoard():void {
            tetromino = tetrominoCreator.getNextTetromino();
            board.addChild(tetromino);
        }
    }
}
