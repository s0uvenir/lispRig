# -*- coding: utf-8 -*-
"""
Created on Sun Mar  3 09:43:08 2013

@author: ben
"""
import numpy as np
import matplotlib as mpl
import matplotlib.pyplot as plt
import mpl_toolkits.mplot3d.axes3d as a3d
import itertools;
import sys
import csv



def procfile(name):
    phil=open(name)
    op=phil.readline()
    if op.strip()[0:7]=="boxplot":
	horizdata=[[float(i) for i in line.split(", ")] for line in phil.readlines()]
	return (horizdata, op.strip())
    readr=csv.reader(phil)
    horizdata=[[float(i) for i in line] for line in readr]
    if op.strip()[0:7]=="genplot":
        return (horizdata, op.strip())
    phil.close()
    #print(horizdata)
    dim=horizdata[0].__len__()
    vertdata=[]
    for i in range(dim): 
        vertdata.append([])
    for point in horizdata:
        for i in range(dim):
            vertdata[i].append(point[i])
    #print(vertdata)
    return (vertdata, op.strip())
  
def scatter_plot_3d(ax3, data, size=20, color='b'):
    return ax3.scatter(data[0],data[1],data[2], s=size, c=color)
    
def line_plot_3d(ax3, data, style='b-'):
    return ax3.plot(data[0],data[1],data[2], style)
    
def scatter_plot_2d(ax2, data, size=20, color='b'):
    return ax2.scatter(data[0],data[1], s=size, c=color)
    
def line_plot_2d(ax2, data, style='b-'):
    return ax2.plot(data[0],data[1], style)
    

if __name__=='__main__':
    try: 
        (data, op_string)=procfile(sys.argv[1])
    except Exception as e:
	print(e)
        while True:
            fn=raw_input("Please enter a filename: ")
            try:
                (data, op_string)=procfile(fn)
            except:
                continue
            break
        
    fig=plt.figure()
    c=itertools.cycle(['b','g','r','c','m','y','k'])
    s=itertools.cycle(['b-','g-','r-','c-','m-','y-','k-'])
    
    op_lst=op_string.split(',')
    op_lst=[i.strip() for i in op_lst]
    op_string=op_lst[0]
    
    if op_string=="scatter3": 
        ax=a3d.Axes3D(fig)
        scatter_plot_3d(ax, data)
    
    if op_string=="line3": 
        ax=a3d.Axes3D(fig)
        line_plot_3d(ax, data)
    
    if op_string=="scatter2":
        ax=fig.add_subplot(111)
        for i in range(data.__len__()-1):
            scatter_plot_2d(ax, [data[0], data[i+1]], color=c.next())
            
    if op_string=="line2":
        ax=fig.add_subplot(111)
        for i in range(data.__len__()-1):
            line_plot_2d(ax, [data[0], data[i+1]], style=s.next())
            
    if op_string=="boxplot":
        ax=fig.add_subplot(111)
        ax.boxplot(data)
    
    if op_string=="genplot":
        ax=fig.add_subplot(111)
        data=[sorted(filter(lambda x: x>0, i)) for i in data]
        Lextreme=[]
        Lquad=[]
        Med=[]
        Hquad=[]
        Hextreme=[]
        for datum in data:
            l=datum.__len__()
            Lextreme.append(datum[0])
            Lquad.append(datum[int(l/4)])
            Med.append(datum[int(l/2)])
            Hquad.append(datum[int(3*l/4)])
            Hextreme.append(datum[-1])
        for item in [Hextreme, Hquad, Med, Lquad, Lextreme]:
            line_plot_2d(ax, [range(item.__len__()), item], style=s.next())
        ax.legend(('100%', '75%', '50%', '25%', '0%'), loc='lower right')
        
    try:
        plt.title(op_lst[1])
    except:
        pass

    if (np.mean(Med)<0.01 or np.mean(Med)>0.99) and max(Med)<=1:
	ax.set_yscale('log')
    plt.draw()
    #fig.show() 
    
    try:
        if sys.argv[2]=='-s':
            outname=sys.argv[1].split('.')[0]+'.png'
            plt.savefig(outname)
    except:
        pass
    
    print("\n\nIf you used -i, you're now in a Python environment.\nYou can use ax.set_xlim(l,h) and ax.set_xlabel(str),")
    print("but you may need to plt.draw() or fig.show() to update changes.\nUse exit() to quit.\n")
    
    

