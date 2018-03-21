package com.farmersedge.aries.watershed;

import java.util.Arrays;
import java.util.List;
import java.util.Vector;

public class WatershedSegmenter {
    int g_w;
    int g_h;

    public Double[] calculate(List<Double> grayList, int width, int height,
                              int floodPoints, int windowWidth,
                              int connectedPixels) {

        Double[] map = new Double[width*height];
        grayList.toArray(map);

     //   Double [] map = (Double []) grayList.toArray();
        g_w = width;
        g_h = height;
        // height map is the gray color image
        // LUT is the lookup table for the processed pixels
        Double[] lut = new Double[g_w*g_h];
        // fill LUT with ones
        Arrays.fill(lut, 1.0);
        Vector<FloodPoint> minimums = new Vector<FloodPoint>();
        // loop all the pixels of the image until
        // a) all the required minimums have been found
        // OR
        // b) there are no more unprocessed pixels left
        Double foundMinimums = 0.0;
        while (foundMinimums<floodPoints) {
            Double minimumValue = 256.0;
            int minimumPosition = -1;
            for (int i=0;i<lut.length;i++) {
                if ((lut[i]==1)&&(map[i]<minimumValue)) {
                    minimumPosition = i;
                    minimumValue = map[i];
                }
            }
            // check if minimum was found
            if (minimumPosition!=-1) {
                // add minimum to found minimum vector
                int x = minimumPosition%g_w;
                int y = minimumPosition/g_w;
                Double grey = map[x+g_w*y];
                Double label = foundMinimums;
                minimums.add(new FloodPoint(x,y,
                        label,grey));
                // remove pixels around so that the next minimum
                // must be at least windowWidth/2 distance from
                // this minimum (using square, could be circle...)
                int half = windowWidth/2;
                fill(x-half,y-half,x+half,y+half,lut,0.0);
                lut[minimumPosition] = 0.0;
                foundMinimums++;
            } else {
                // stop while loop
                System.out.println("Out of pixels. Found "
                        + minimums.size()
                        + " minimums of requested "
                        + floodPoints+".");
                break;
            }
        }

        // start flooding from minimums
        lut = flood(map,minimums,connectedPixels);

        return lut;
    }
    private Double[] flood(Double[] map, Vector<FloodPoint> minimums,
                        int connectedPixels) {
        SortedVector queue = new SortedVector();
        //BufferedImage result = new BufferedImage(g_w, g_h,
        //        BufferedImage.TYPE_INT_RGB);
        Double[] lut = new Double[g_w*g_h];
        int[] inqueue = new int[g_w*g_h];
        // not processed = -1, processed >= 0
        Arrays.fill(lut, -1.0);
        Arrays.fill(inqueue, 0);
        // Initialize queue with each found minimum
        for (int i=0;i<minimums.size();i++) {
            FloodPoint p = minimums.elementAt(i);
            Double label = p.label;
            // insert starting pixels around minimums
            addPoint(queue, inqueue, map, p.x,   p.y-1, label);
            addPoint(queue, inqueue, map, p.x+1, p.y,   label);
            addPoint(queue, inqueue, map, p.x,   p.y+1, label);
            addPoint(queue, inqueue, map, p.x-1, p.y,   label);
            if (connectedPixels==8) {
                addPoint(queue, inqueue, map, p.x-1, p.y-1, label);
                addPoint(queue, inqueue, map, p.x+1, p.y-1, label);
                addPoint(queue, inqueue, map, p.x+1, p.y+1, label);
                addPoint(queue, inqueue, map, p.x-1, p.y+1, label);
            }
            int pos = p.x+p.y*g_w;
            lut[pos] = label;
            inqueue[pos] = 1;
        }

        // start flooding
        while (queue.size()>0) {
            // find minimum
            FloodPoint extracted = null;
            // remove the minimum from the queue
            extracted = (FloodPoint)queue.remove(0);
            int x = extracted.x;
            int y = extracted.y;
            Double label = extracted.label;
            // check pixels around extracted pixel
            Double[] labels = new Double[connectedPixels];
            labels[0] = getLabel(lut,x,y-1);
            labels[1] = getLabel(lut,x+1,y);
            labels[2] = getLabel(lut,x,y+1);
            labels[3] = getLabel(lut,x-1,y);
            if (connectedPixels==8) {
                labels[4] = getLabel(lut,x-1,y-1);
                labels[5] = getLabel(lut,x+1,y-1);
                labels[6] = getLabel(lut,x+1,y+1);
                labels[7] = getLabel(lut,x-1,y+1);
            }
            boolean onEdge = isEdge(labels,extracted);
            if (onEdge) {
                // leave edges without label
            } else {
                // set pixel with label
                lut[x+g_w*y] = extracted.label;
            }
            if (!inQueue(inqueue,x,y-1)) {
                addPoint(queue, inqueue, map, x, y-1, label);
            }
            if (!inQueue(inqueue,x+1,y)) {
                addPoint(queue, inqueue, map, x+1, y, label);
            }
            if (!inQueue(inqueue,x,y+1)) {
                addPoint(queue, inqueue, map, x, y+1, label);
            }
            if (!inQueue(inqueue,x-1,y)) {
                addPoint(queue, inqueue, map, x-1, y, label);
            }
            if (connectedPixels==8) {
                if (!inQueue(inqueue,x-1,y-1)) {
                    addPoint(queue, inqueue, map, x-1, y-1, label);
                }
                if (!inQueue(inqueue,x+1,y-1)) {
                    addPoint(queue, inqueue, map, x+1, y-1, label);
                }
                if (!inQueue(inqueue,x+1,y+1)) {
                    addPoint(queue, inqueue, map, x+1, y+1, label);
                }
                if (!inQueue(inqueue,x-1,y+1)) {
                    addPoint(queue, inqueue, map, x-1, y+1, label);
                }
            }
          }
        return lut;
    }

    private boolean inQueue(int[] inqueue, int x, int y) {
        if (x<0||x>=g_w||y<0||y>=g_h) {
            return false;
        }
        if (inqueue[x+g_w*y] == 1) {
            return true;
        }
        return false;
    }
    private boolean isEdge(Double[] labels, FloodPoint extracted) {
        for (int i=0;i<labels.length;i++) {
            if (labels[i]!=extracted.label&&labels[i]!=-1) {
                return true;
            }
        }
        return false;
    }
    private Double getLabel(Double[] lut, int x, int y) {
        if (x<0||x>=g_w||y<0||y>=g_h) {
            return -2.0;
        }
        return lut[x+g_w*y];
    }

    private void addPoint(SortedVector queue,
                          int[] inqueue, Double[] map,
                          int x, int y, Double label) {
        if (x<0||x>=g_w||y<0||y>=g_h) {
            return;
        }
        queue.add(new FloodPoint(x,y,label,map[x+g_w*y]));
        inqueue[x+g_w*y] = 1;
    }

    private void fill(int x1, int y1, int x2, int y2,
                      Double[] array, Double value) {
        for (int y=y1;y<y2;y++) {
            for (int x=x1;x<x2;x++) {
                // clip to boundaries
                if (y>=0&&x>=0&&y<g_h&&x<g_w) {
                    array[x+g_w*y] = value;
                }
            }
        }
    }


    class FloodPoint implements Comparable<Object> {
        int x;
        int y;
        Double label;
        Double grey;

        public FloodPoint(int x, int y, Double label, Double grey) {
            this.x = x;
            this.y = y;
            this.label = label;
            this.grey = grey;
        }
        @Override
        public int compareTo(Object o) {
            FloodPoint other = (FloodPoint)o;
            if (this.grey < other.grey ) {
                return -1;
            } else if (this.grey > other.grey ) {
                return 1;
            }
            return 0;
        }
    }

}