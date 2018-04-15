# -*- coding: utf-8 -*-
"""
Created on Fri Mar 23 18:28:45 2018

@author: Ken
"""
import argparse 
import sys
import tensorflow as tf
import numpy as np
#constants
bs = 7

# 1x9x9x2 binary image input



#output to be [None,bs,bs,4]
def layer (nnInput,chan,stride,skipInput=None):
    c = tf.contrib.layers.conv2d(nnInput,chan,stride)
    n = tf.contrib.layers.batch_norm(c,is_training=False)
    if None != skipInput:
        n = n+skipInput
    r = tf.nn.relu(n)
    return r

#output to be [None,bs,bs,4]
def residual(nnInput):
    l = layer(nnInput,4,3)
    r = layer(l,4,3,nnInput,)
    return r
    
def headMove(nnInput):
    l = layer(nnInput,2,1)
    logits = tf.contrib.layers.fully_connected(l,bs*bs)
    return logits

def headScore(nnInput):
    l = layer(nnInput,1,1)
    l = tf.reshape(l,[-1,bs*bs])
    rawScore = tf.contrib.layers.fully_connected(l,1)    
    score = tf.nn.tanh(rawScore)
    return score

def infer(tIn):   
    tf.reset_default_graph()
    
    x_const = tf.constant(tIn,shape=tIn.shape)
    initial = layer(x_const,4,3)
    res=initial
    for i in range(10):   
        res = residual(res)
    score = headScore(res)
    sess = tf.Session()
    sess.run(tf.global_variables_initializer())
    with sess.as_default():
        s = score.eval()


def test():
    infer(np.random.rand(1000,bs,bs,2).astype(np.float32))

def cmdParser():
    usageExample = '''
example:    
python alpha1.py create > mlmodel
python alpha1.py infer -load=mlmodel < infer.msgpack > probsAndScore.msgpack
python alpha1.py train -load=mlmodel < train.msgpack > mlmodel
python alpha1.py debug        
    '''
    parserMode= argparse.ArgumentParser(description="alpha1go ML setup"
        ,formatter_class=argparse.RawDescriptionHelpFormatter
        ,epilog=usageExample)        
    subParsers = parserMode.add_subparsers(help='modes:')    
    subCreate = subParsers.add_parser('create',help='create -h')
    subCreate.add_argument('-layers',type=int,default=2,help="input layers")
    subCreate.add_argument('-filters',type=int,default=4,help="filters per residual layer")
    subCreate.add_argument('-residuals',type=int,default=3,help="residual layers")
    subCreate.add_argument('-rows',type=int,default=7,help="input rows")
    subCreate.add_argument('-columns',type=int,default=7,help="input columns")
    subCreate.add_argument('-probs',type=int,default=49,help="output probability count")
    subInfer = subParsers.add_parser('infer',help='infer -h')
    subInfer.add_argument('-load',type=argparse.FileType('r'))
    subTrain = subParsers.add_parser('train',help='train -h')
    subTrain.add_argument('-loadx',type=argparse.FileType('r'))
    subDebug = subParsers.add_parser('debug',help='debug -h')
    return parserMode
            
def main():
    parser = cmdParser();            
    args = parser.parse_args()
    print (args)
main()    
    
    
