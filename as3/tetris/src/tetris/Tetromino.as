package tetris {

    import flash.display.Sprite;

    public class Tetromino extends Sprite {

        public function Tetromino():void {
            this.x = (Board.SIZE.x / 2) * Board.BLOCK_SIZE;
            this.y = Board.BLOCK_SIZE;
        }

        public function draw():void {
            graphics.beginFill(0x00FF00); // green color
            graphics.drawRect(-Board.BLOCK_SIZE, -Board.BLOCK_SIZE,
                    Board.BLOCK_SIZE * 2, Board.BLOCK_SIZE * 2);
            graphics.endFill();
        }

        public function moveOneBlockDown():void {
            this.y += Board.BLOCK_SIZE;
        }

        public function moveLeft():void {
            this.x -= Board.BLOCK_SIZE;
        }

        public function moveRight():void {
            this.x += Board.BLOCK_SIZE;
        }
    }
}
