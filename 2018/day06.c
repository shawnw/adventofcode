#include <limits.h>
#include <stdio.h>
#include <stdlib.h>

struct coord {
  int id;
  int x;
  int y;
  int area;
};

int max_x(struct coord *c, int len) {
  int mx = INT_MIN;
#pragma omp simd reduction(max:mx)
  for (int i = 0; i < len; i += 1) {
    if (c[i].x > mx) {
      mx = c[i].x;
    }
  }
  return mx;
}

int max_y(struct coord *c, int len) {
  int my = INT_MIN;
#pragma omp simd reduction(max:my)
  for (int i = 0; i < len; i += 1) {
    if (c[i].y > my) {
      my = c[i].y;
    }
  }
  return my;
}

int min_x(struct coord *c, int len) {
  int mx = INT_MAX;
#pragma omp simd reduction(min:mx)
  for (int i = 0; i < len; i += 1) {
    if (c[i].x < mx) {
      mx = c[i].x;
    }
  }
  return mx;
}

int min_y(struct coord *c, int len) {
  int my = INT_MAX;
#pragma omp simd reduction(min:my)
  for (int i = 1; i < len; i += 1) {
    if (c[i].y < my) {
      my = c[i].y;
    }
  }
  return my;
}

struct grid_point {
  int x;
  int y;
  int dist;
  int nearest_id;
};

int manhatten(struct coord a, struct grid_point b) {
  return abs(a.x - b.x) + abs(a.y - b.y);
}

int find_area(struct grid_point **grid, int maxx, int maxy, int id) {
  int area = 0;
#pragma omp simd reduction(+:area) collapse(2)
  for (int x = 0; x < maxx; x += 1) {
    for (int y = 0; y < maxy; y += 1) {
      if (grid[x][y].nearest_id == id) {
        area += 1;
      }
    }
  }
  return area;
}

/* Input has 50 pairs */
#define MAX_POINTS 50

int main(void) {
  struct coord points[MAX_POINTS];
  int len = 0;

  while (scanf("%d, %d", &points[len].x, &points[len].y) == 2) {
    points[len].id = len + 1;
    points[len].area = 0;
    len += 1;
    if (len > MAX_POINTS) {
      fputs("Input too long!\n", stderr);
      return EXIT_FAILURE;
    }
  }

  int maxx = max_x(points, len) + 100;
  int maxy = max_y(points, len) + 100;

  struct grid_point **grid = calloc(sizeof(struct grid_point *), maxx);
  for (int i = 0; i < maxx; i += 1) {
    grid[i] = calloc(sizeof(struct grid_point), maxy);
    for (int j = 0; j < maxy; j += 1) {
      grid[i][j].x = i - 50;
      grid[i][j].y = j - 50;
      grid[i][j].dist = INT_MAX;
      grid[i][j].nearest_id = 0;
    }
  }

  struct grid_point **offsetgrid = calloc(sizeof(struct grid_point *), maxx);
  for (int i = 0; i < maxx; i += 1) {
    offsetgrid[i] = grid[i] + 50;
  }
  struct grid_point **ogrid = offsetgrid + 50;

  for (int x = -50; x < maxx - 50; x += 1) {
    for (int y = -50; y < maxy - 50; y += 1) {
      for (int i = 0; i < len; i += 1) {
        int d = manhatten(points[i], ogrid[x][y]);
        if (d == ogrid[x][y].dist) {
          ogrid[x][y].nearest_id = -1;
        } else if (d < ogrid[x][y].dist) {
          ogrid[x][y].dist = d;
          ogrid[x][y].nearest_id = i + 1;
        }
      }
    }
  }

  for (int y = 0; y < maxy; y += 1) {
    if (grid[0][y].nearest_id > 0) {
      points[grid[0][y].nearest_id - 1].area = INT_MIN;
    }
    if (grid[maxx - 1][y].nearest_id > 0) {
      points[grid[maxx - 1][y].nearest_id - 1].area = INT_MIN;
    }
  }

  for (int x = 0; x < maxx; x += 1) {
    if (grid[x][0].nearest_id > 0) {
      points[grid[x][0].nearest_id - 1].area = INT_MIN;
    }
    if (grid[x][maxy - 1].nearest_id > 0) {
      points[grid[x][maxy - 1].nearest_id - 1].area = INT_MIN;
    }
  }

  int max_area = INT_MIN;
  for (int i = 0; i < len; i += 1) {
    if (points[i].area == INT_MIN) {
      continue;
    }

    int area = find_area(grid, maxx, maxy, points[i].id);
    if (area > max_area) {
      max_area = area;
    }
  }

  printf("Part 1: %d\n", max_area);

  free(offsetgrid);
  for (int x = 0; x < maxx; x += 1) {
    free(grid[x]);
  }
  free(grid);

  int minx = min_x(points, len);
  int miny = min_y(points, len);
  maxx -= 100;
  maxy -= 100;

#define DIST 10000

  int total_in_range = 0;
#pragma omp parallel for reduction(+:total_in_range) schedule(dynamic)
  for (int x = minx - DIST; x < maxx + DIST; x += 1) {
    for (int y = miny - DIST; y < maxy + DIST; y += 1) {
      int distance = 0;
      _Bool all_in_range = 1;
      for (int i = 0; i < len; i += 1) {
        distance += abs(points[i].x - x) + abs(points[i].y - y);
        if (distance >= DIST) {
          all_in_range = 0;
          break;
        }
      }
      if (all_in_range) {
        total_in_range += 1;
      }
    }
  }

  printf("Part 2: %d\n", total_in_range);

  return 0;
}
