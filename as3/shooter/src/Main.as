package
{
    import flash.display.Sprite;
    import ships.ShipCreator;

    public class Main extends Sprite
    {
        public function Main()
        {
            // instantiate ship creator
            var shipFactory:ShipCreator = new ShipCreator();
            // place hero ship
            shipFactory.addShip(ShipCreator.HERO, this.stage,
                    this.stage.stageWidth / 2, this.stage.stageHeight - 120);
            // place alien ships
            for (var i:Number = 0; i < 5; i++)
            {
                shipFactory.addShip(ShipCreator.ALIEN, this.stage,
                        100 + 80 * i, 50);
            }
        }
    }
}
