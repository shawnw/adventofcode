#include <string>
#include <iostream>

int main(int argc, char *argv[])
{
  std::string in (argv[1]);
  size_t total (0);
  for (int i=0; i<in.size(); ++i)
    {
      if (in[i] == in[(i+(in.size()/2))%in.size()])
        {
          total += in[i]-'0';
        }
    }
  std::cout << "total: " << total << "\n";
}
