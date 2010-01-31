package {

    import flash.display.Shape;
    import flash.events.Event;

    public class MovingCircle extends Shape {

        public var xspeed:Number;
        public var yspeed:Number;

        public function MovingCircle() {
            // graphics is an inherited property from Shape
            graphics.beginFill(0xff9933, 1);
            graphics.drawCircle(0, 0, 40);
            trace("Narysowano poruszające się koło.");
        }

        // initialization, called after parent addChild

        public function init():void {
            // x, y and stage are inherited properties

            //x = Math.random() * (stage.stageWidth);
            //y = Math.random() * (stage.stageHeight);
            x = Math.random() * 200;
            y = Math.random() * 200;

            xspeed = Math.random() * 10;
            yspeed = Math.random() * 10;

            // start step triggering function based on enterframe event
            addEventListener(Event.ENTER_FRAME, step);
        }

        public function step(event:Event):void {

            // bounce ball at stage edges

            if (x + xspeed > stage.stageWidth) xspeed *= -1;
            else if (x + xspeed < 0) xspeed *= -1;

            if (y + yspeed > stage.stageHeight) yspeed *= -1;
            else if (y + yspeed < 0) yspeed *= -1;

            // set position
            x += xspeed;
            y += yspeed;
        }
    }
}
