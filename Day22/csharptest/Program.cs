using System;
using System.Collections.Generic;

public class Program
{
    private static readonly int s_depth = 10914;
    private static readonly (int x, int y) s_target = (9,739);
    public static void Main()
    {
        int risk = 0;
        for (int y = 0; y <= s_target.y; y++)
        {
            for (int x = 0; x <= s_target.x; x++)
            {
                switch (GetRegionType(x, y))
                {
                    case 'W': risk++; break;
                    case 'N': risk += 2; break;
                }
            }
        }
        Console.WriteLine(risk);

        BfsSolve();
    }

    private static void BfsSolve()
    {
        (int x, int y)[] neis = { (-1, 0), (0, 1), (1, 0), (0, -1) };

        Queue<(int x, int y, char tool, int switching, int minutes)> queue = new Queue<(int x, int y, char tool, int switching, int minutes)>();
        HashSet<(int x, int y, char tool)> seen = new HashSet<(int x, int y, char tool)>();
        queue.Enqueue((0, 0, 'T', 0, 0));
        seen.Add((0, 0, 'T'));
        while (queue.Count > 0)
        {
            (int x, int y, char tool, int switching, int minutes) = queue.Dequeue();
            if (switching > 0)
            {
                if (switching != 1 || seen.Add((x, y, tool)))
                    queue.Enqueue((x, y, tool, switching - 1, minutes + 1));
                continue;
            }

            if ((x, y) == s_target && tool == 'T')
            {
                Console.WriteLine(minutes);
                break;
            }

            foreach ((int xo, int yo) in neis)
            {
                (int nx, int ny) = (x + xo, y + yo);
                if (nx < 0 || ny < 0)
                    continue;

                if (GetAllowedTools(GetRegionType(nx, ny)).Contains(tool) && seen.Add((nx, ny, tool)))
                    queue.Enqueue((nx, ny, tool, 0, minutes + 1));
            }

            foreach (char otherTool in GetAllowedTools(GetRegionType(x, y)))
                queue.Enqueue((x, y, otherTool, 6, minutes + 1));
        }
    }

    private static readonly Dictionary<(int x, int y), int> s_erosionLevels = new Dictionary<(int x, int y), int>();
    private static int ErosionLevel(int x, int y)
    {
        if (s_erosionLevels.TryGetValue((x, y), out int val))
            return val;

        if ((x, y) == (0, 0))
            val = 0;
        else if ((x, y) == s_target)
            val = 0;
        else if (y == 0)
            val = x * 16807;
        else if (x == 0)
            val = y * 48271;
        else
            val = ErosionLevel(x - 1, y) * ErosionLevel(x, y - 1);

        val += s_depth;
        val %= 20183;
        s_erosionLevels.Add((x, y), val);
        return val;
    }

    private static char GetRegionType(int x, int y)
    {
        int erosionLevel = ErosionLevel(x, y);
        return "RWN"[erosionLevel % 3];
    }

    private static string GetAllowedTools(char region)
    {
        switch (region)
        {
            case 'R': return "CT";
            case 'W': return "CN";
            case 'N': return "TN";
            default: throw new Exception("Unreachable");
        }
    }
}