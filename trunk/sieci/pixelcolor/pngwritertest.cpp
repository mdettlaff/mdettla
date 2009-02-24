#include <pngwriter.h>


int main(int argc, char * argv[])
{
   pngwriter image1(1,1,0,"scaled_k.png");
   pngwriter image2(1,1,0,"scaled_kxky.png");
   pngwriter image3(1,1,0,"scaled_wh.png");
   
   image1.readfromfile(argv[1]);
   image2.readfromfile(argv[1]);
   image3.readfromfile(argv[1]);

   std::cout << "Read file is:" << image1.getheight() << " " << image1.getwidth() << "\n";   
   std::cout << "Done read\n";   
   
   image1.scale_k(0.7);
   std::cout << "New file is:" << image1.getheight() << " " << image1.getwidth() << "\n";   
   std::cout << image1.read(100,100,1) << std::endl;
   image1.close();
   
   std::cout << "Done1\n";   
   
   image2.scale_kxky(0.7, 0.3);
   image2.close();
   
   std::cout << "Done2\n";   
   
   image3.scale_wh(400,100);
   image3.close();
   
   std::cout << "Done3\n";   
   
   
   
   
   
   return 0;   
}
