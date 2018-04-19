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
headScoreName = "headScore"
headProbsName = "headProbs"
inferName = "inferName"
isTrainingName = "isTrainingBool"
# 1x9x9x2 binary image input
isTrainingVar=None

def layer (nnInput,chan,stride,skipInput=None):
    c = tf.contrib.layers.conv2d(nnInput,chan,stride)
    n = tf.contrib.layers.batch_norm(c,is_training=isTrainingVar)
    if None != skipInput:
        n = n+skipInput
    r = tf.nn.relu(n)
    return r

def residual(nnInput):
    l = layer(nnInput,4,3)
    r = layer(l,4,3,nnInput,)
    return r
    
def headMove(nnInput,cntProbs):
    l = layer(nnInput,2,1)
    logits = tf.contrib.layers.fully_connected(l,cntProbs)
    probs = tf.nn.softmax(logits,name=headProbsName)
    return probs

def headScore(nnInput,cntProbs):
    l = layer(nnInput,1,1)
    l = tf.reshape(l,[-1,cntProbs])
    rawScore = tf.contrib.layers.fully_connected(l,1)    
    score = tf.nn.tanh(rawScore,name=headScoreName)
    return score

def createModel(args):
    tf.reset_default_graph()
    global isTrainingVar 
    isTrainingVar = tf.Variable(False,trainable=False,name=isTrainingName,dtype=tf.bool)
#    x_const = tf.constant(tIn,shape=tIn.shape)
    x_pl = tf.placeholder(tf.float32,(None,args.rows,args.columns,args.layers),name=inferName)
    initial = layer(x_pl,args.filters,3)
    res=initial
    for i in range(args.residuals):   
        res = residual(res)
    score = headScore(res,args.probs)
    probs = headMove(res,args.probs)
    sess = tf.Session()
    sess.run(tf.global_variables_initializer())
    saver = tf.train.Saver()
    saver.save(sess, args.model)
      

def load(args):   
    sess=tf.Session()    
    #First let's load meta graph and restore weights
    saver = tf.train.import_meta_graph(args.model+".meta")
    saver.restore(sess,args.model)
    return sess,saver
    
def infer(args,tIn):
    sess,saver = load(args)
    graph = tf.get_default_graph()
    x = graph.get_tensor_by_name(inferName+":0")
    isTrainingVar = graph.get_tensor_by_name(isTrainingName+":0")    
    sess.run(tf.assign(isTrainingVar,False))
    headScore = graph.get_tensor_by_name(headScoreName+":0")    
    headProbs = graph.get_tensor_by_name(headProbsName+":0")  
    result = sess.run([headScore,headProbs],feed_dict={x:tIn})
    print (result)

def testInfer(args):
    r = np.random.rand(1,7,7,2).astype(np.float32)
#    tIn = tf.constant(r,shape=r.shape)
    infer(args,r)

def testTrain(args):
    sess,saver = load(args)    
    graph = tf.get_default_graph()
    isTrainingVar = graph.get_tensor_by_name(isTrainingName+":0")    
    sess.run(tf.assign(isTrainingVar,True))
    

def cmdParser():
    modelPath =     "./model/a1Model"
    
    usageExample = '''
example:    
python alpha1.py create 
python alpha1.py infer -load=mlmodel < infer.msgpack > probsAndScore.msgpack
python alpha1.py train -load=mlmodel < train.msgpack > mlmodel
python alpha1.py debug        
    '''
    parserMode= argparse.ArgumentParser(description="alpha1go ML setup"
        ,formatter_class=argparse.RawDescriptionHelpFormatter
        ,epilog=usageExample)        
    subParsers = parserMode.add_subparsers(help='modes:',dest='mode')    
    subCreate = subParsers.add_parser('create',help='create -h')
    subCreate.add_argument('-layers',type=int,default=2,help="input layers")
    subCreate.add_argument('-filters',type=int,default=4,help="filters per residual layer")
    subCreate.add_argument('-residuals',type=int,default=3,help="residual layers")
    subCreate.add_argument('-rows',type=int,default=7,help="input rows")
    subCreate.add_argument('-columns',type=int,default=7,help="input columns")
    subCreate.add_argument('-probs',type=int,default=49,help="output probability count")
    subCreate.add_argument('-model',default=modelPath,help="output Model dir")
    subInfer = subParsers.add_parser('infer',help='infer -h')
    subInfer.add_argument('-model',default=modelPath,help="input Model dir")
    subTrain = subParsers.add_parser('train',help='train -h')
    subTrain.add_argument('-model',default=modelPath,help="input & output Model dir")
    return parserMode
            
def main():
    parser = cmdParser();            
    args = parser.parse_args()
    print (args)
    if args.mode == 'create' :
        createModel(args)
    elif args.mode == 'infer' :
        testInfer(args)
main()    
    
    
