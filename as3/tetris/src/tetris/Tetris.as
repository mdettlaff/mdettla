package tetris {

    import flash.display.DisplayObjectContainer;
    import flash.display.Sprite;
    import flash.events.KeyboardEvent;
    import flash.events.TimerEvent;
    import flash.ui.Keyboard;
    import flash.utils.Timer;

    public class Tetris extends Sprite {

        private static const SPEED:int = 1000;

        private var board:Board;
        private var tetrominoCreator:TetrominoCreator;
        private var tetromino:Tetromino;

        public function Tetris(mainContainer:DisplayObjectContainer) {
            board = new Board();
            tetrominoCreator = new TetrominoCreator();
            nextTetromino();

            board.addChild(tetromino);
            mainContainer.addChild(board);

            var timer:Timer = new Timer(SPEED, 0);
            timer.addEventListener(TimerEvent.TIMER, timerHandler);
            timer.start();

            mainContainer.addEventListener(KeyboardEvent.KEY_DOWN, keyHandler);
            board.addEventListener(
                    TetrisEvent.TETROMINO_LANDED, nextTetromino);
            board.addEventListener(TetrisEvent.NEW_GAME, nextTetromino);
        }

        private function timerHandler(event:TimerEvent):void {
            tetromino.moveDown();
        }

        private function keyHandler(event:KeyboardEvent):void {
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
                case 78: // N
                    board.dispatchEvent(new TetrisEvent(TetrisEvent.NEW_GAME));
                    break;
            }
        }

        private function nextTetromino(event:TetrisEvent = null):void {
            if (tetromino != null) {
                board.removeChild(tetromino);
            }
            tetromino = tetrominoCreator.getNextTetromino();
            board.addChild(tetromino);
        }
    }
}
