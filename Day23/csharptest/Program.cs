using System;

namespace csharptest
{
    class Program
    {
        static void Main(string[] args)
        {
            var bots = input.Split(new string[] { "\n", "pos=<", ",", ">", ", ", " ", "r=", "\n" }, StringSplitOptions.RemoveEmptyEntries).Select(x => int.Parse(x)).ToArray();
            var nanoBots = new List<NanoBots>();
            var bot = new NanoBots();
            
            long distance = 1;
            for (var i = 0; i < bots.Count(); i += 4)
            {
                nanoBots.Add(new NanoBots
                {
                    Pos = (bots[i], bots[i + 1], bots[i + 2]),
                    Radius = bots[i + 3]
                });
            }
            var xs = new List<long>();
            var ys = new List<long>();
            var zs = new List<long>();
            foreach(var nanoBot in nanoBots)
            {
                xs.Add(nanoBot.Pos.x);
                ys.Add(nanoBot.Pos.y);
                zs.Add(nanoBot.Pos.z);
            }
            xs.Add(0);
            ys.Add(0);
            zs.Add(0);

            while(distance < xs.Max() - xs.Min())
            {
                distance *= 2;
            }
            while(true)
            {
                var targetCount = 0;
                var bestBot = (x: (long)0, y: (long)0, z: (long)0);
                long maxBot = 0;
                for(var x = xs.Min(); x < xs.Max() + 1; x+=distance)
                {
                    for(var y = ys.Min(); y < ys.Max() + 1; y+=distance)
                    {
                        for(var z = zs.Min(); z < zs.Max() + 1; z+=distance)
                        {
                            var count = 0;
                            foreach(var nanoBot in nanoBots)
                            {
                                var calc = Math.Abs(x - nanoBot.Pos.x) + Math.Abs(y - nanoBot.Pos.y) + Math.Abs(z - nanoBot.Pos.z);
                                if(((calc - nanoBot.Radius) / distance) <= 0)
                                {
                                    count++;
                                }
                            }
                            if(count > targetCount)
                            {
                                targetCount = count;
                                maxBot = Math.Abs(x) + Math.Abs(y) + Math.Abs(z);
                                bestBot = (x, y, z);
                            }
                            else if (count == targetCount)
                            {
                                if(maxBot == 0 || Math.Abs(x) + Math.Abs(y) + Math.Abs(z) < maxBot)
                                {
                                    maxBot = Math.Abs(x) + Math.Abs(y) + Math.Abs(z);
                                    bestBot = (x, y, z);
                                }
                            }
                        }
                    }
                }
                if(distance == 1)
                {
                    Console.WriteLine($"The max count I have found so far: {targetCount}");
                    return maxBot;
                }
                else
                {
                    xs.Clear();
                    ys.Clear();
                    zs.Clear();
                    xs.Add(bestBot.x - distance);
                    xs.Add(bestBot.x + distance);
                    ys.Add(bestBot.y - distance);
                    ys.Add(bestBot.y + distance);
                    zs.Add(bestBot.z - distance);
                    zs.Add(bestBot.z + distance);
                    distance /= 2;
                }
            }
        }
    }
}
