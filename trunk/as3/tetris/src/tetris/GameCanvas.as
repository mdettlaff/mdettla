package tetris {

    import flash.display.DisplayObject;
    import flash.events.Event;

    import mx.core.UIComponent;
    import mx.managers.IFocusManagerComponent;

    public class GameCanvas
        extends UIComponent
        implements IFocusManagerComponent {

        public function GameCanvas() {
            super();
        }

        public function describeChildren(event:Event):void {
            var desc:String = "";
            var child:DisplayObject;
            for (var i:int=0; i < this.numChildren; i++) {
                child = this.getChildAt(i);
                desc += i + ": " + child + '\n';
            }
            trace(desc);
        }
    }
}
