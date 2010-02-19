package tt {

    import flash.display.DisplayObjectContainer;
    import flash.display.Sprite;
    import flash.geom.Rectangle;
    import flash.text.TextField;
    import flash.text.TextFormat;

    public class TypingArea extends Sprite {

        public static const MAX_LINES:int = 6;
        public static const LINE_HEIGHT:int = 44;
        public static const TEXT_TO_TYPE_COLOR:uint = 0x000000;
        public static const WRITTEN_TEXT_COLOR:uint = 0x0000C0;

        // text to type
        private var visibleTextLines:Array /* of String */;
        // text typed in by the user
        private var visibleWrittenLines:Array /* of String */;

        private var bounds:Rectangle;
        private var lineColor:uint;
        private var fillColor:uint;

        public function TypingArea(width:int, height:int,
                fillColor:uint = 0xFFFFFF, lineColor:uint = 0x000000) {
            this.fillColor = fillColor;
            this.lineColor = lineColor;
            bounds = new Rectangle(0, 0, width, height);
            drawBounds();

            initTextLines();
            initWrittenLines();
        }

        public function draw(typingTestModel:TypingTestModel):void {
            drawTextLines(typingTestModel.textLines);
            drawWrittenLines(typingTestModel.writtenLines);
        }

        private function initTextLines():void {
            visibleTextLines = [];
            var yShift:int = 4;
            for (var i:int = 0; i < MAX_LINES; i++) {
                var line:TextField = createLine(yShift, TEXT_TO_TYPE_COLOR);
                visibleTextLines.push(line);
                addChild(line);
                yShift += LINE_HEIGHT;
            }
        }

        private function initWrittenLines():void {
            visibleWrittenLines = [];
            var yShift:int = 24;
            for (var i:int = 0; i < MAX_LINES; i++) {
                var line:TextField = createLine(yShift, WRITTEN_TEXT_COLOR);
                visibleWrittenLines.push(line);
                addChild(line);
                yShift += LINE_HEIGHT;
            }
        }

        private function drawTextLines(textLines:Array /* of String */):void {
            for (var i:int = 0; i < visibleTextLines.length; i++) {
                if (i < textLines.length) {
                    visibleTextLines[i].text = textLines[i];
                } else {
                    visibleTextLines[i].text = "";
                }
            }
        }

        private function drawWrittenLines(
                writtenLines:Array /* of String */):void {
            for (var i:int = 0; i < visibleWrittenLines.length; i++) {
                if (i < writtenLines.length) {
                    visibleWrittenLines[i].text = writtenLines[i];
                } else {
                    visibleWrittenLines[i].text = "";
                }
            }
        }

        private function createLine(yShift:int, color:uint):TextField {
            var format:TextFormat = new TextFormat();
            format.font = "Verdana";
            format.size = 15;
            format.color = color;

            var line:TextField = new TextField();
            line.x = 5;
            line.y = yShift;
            line.defaultTextFormat = format;
            line.text = "";
            line.selectable = false;
            line.width = bounds.width - 2;
            return line;
        }

        private function drawBounds():void {
            graphics.clear();
            graphics.lineStyle(1.0, this.lineColor);
            graphics.beginFill(this.fillColor);
            graphics.drawRect(
                    bounds.left, bounds.top,
                    bounds.width, bounds.height);
            graphics.endFill();
        }
    }
}
