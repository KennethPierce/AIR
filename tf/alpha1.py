# -*- coding: utf-8 -*-
"""
Created on Fri Mar 23 18:28:45 2018

@author: Ken
"""
import argparse 
import sys
import tensorflow as tf
import numpy as np
import msgpack 
from http.server import BaseHTTPRequestHandler, HTTPServer
import socketserver
import json
import matplotlib.pyplot as plt





#constants
headScoreName = "headScore"
headProbsName = "headProbs"
inferName = "inferName"
scoreTargetName = "scoreTarget"
probsTargetName = "probsTarget"
costName = "costName"
costRegName = "costRegName"
costProbName = "costProbName"
costScoreName = "costScoreName"
optimizerName = "optimizerName"
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

def residual(nnInput,chan):
    l = layer(nnInput,chan,3)
    r = layer(l,chan,3,nnInput,)
    return r
    
def headMove(nnInput,cntProbs):
    l = layer(nnInput,2,1)
    f = tf.contrib.layers.flatten(l)
    logits = tf.contrib.layers.fully_connected(f,cntProbs)
    probs = tf.nn.softmax(logits,name=headProbsName)
    return probs

def headScore(nnInput,cntProbs):
    l = layer(nnInput,1,1)
    l = tf.reshape(l,[-1,cntProbs])
    rawScore = tf.contrib.layers.fully_connected(l,1)    
    score = tf.nn.tanh(rawScore,name=headScoreName)
    return score

def headCost(score,probs,scoreTarget,probsTarget,rweight):
    s = tf.pow(score-scoreTarget,2) 
    s = tf.add(s,0,name=costScoreName)
    p = 0-probsTarget*tf.log(probs)     
    p = tf.add(p,0,name=costProbName)
    tv = tf.get_collection(tf.GraphKeys.TRAINABLE_VARIABLES)
    print ("weights: ",tv)
    l2s = [tf.nn.l2_loss(i) for i in tv]
    r=tf.add_n(l2s)
    r = r **(0.5)
    r = r * rweight
    r = tf.add(r,0,name=costRegName)
    return tf.add(s,p+r,name=costName)

def createModel(args):
    tf.reset_default_graph()
    global isTrainingVar 
    isTrainingVar = tf.Variable(False,trainable=False,name=isTrainingName,dtype=tf.bool)
#    x_const = tf.constant(tIn,shape=tIn.shape)
    x_pl = tf.placeholder(tf.float32,(None,args.rows,args.columns,args.layers),name=inferName)
    initial = layer(x_pl,args.filters,3)
    res=initial
    for i in range(args.residuals):   
        res = residual(res,args.filters)
    score = headScore(res,args.probs)
    probs = headMove(res,args.probs)
    scoreTarget = tf.placeholder(tf.float32,(None,1),name=scoreTargetName)
    probsTarget = tf.placeholder(tf.float32,(None,args.rows*args.columns),name=probsTargetName)
    cost = headCost(score,probs,scoreTarget,probsTarget,args.rweight)
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

def mkTensor(bl,wh):
    t1 = np.array(bl).astype(np.float32).reshape(1,7,7,1)
    t2 = np.array(wh).astype(np.float32).reshape(1,7,7,1)
    ret = np.concatenate([t1,t2],axis=3)
    assert (ret.shape == (1,7,7,2))
    return ret

    
def inferOnDataFn(args):
    sess,saver = load(args)
    graph = tf.get_default_graph()
    x = graph.get_tensor_by_name(inferName+":0")
    isTrainingVar = graph.get_tensor_by_name(isTrainingName+":0")    
    sess.run(tf.assign(isTrainingVar,False))
    headScore = graph.get_tensor_by_name(headScoreName+":0")    
    headProbs = graph.get_tensor_by_name(headProbsName+":0")  
    def inferOnData(inferFeed):
        result = sess.run([headScore,headProbs],feed_dict={x:inferFeed})
        return result
    #print (result)
    return inferOnData

def mkClass(inferOnData):
    class S(BaseHTTPRequestHandler):
        inferfn = inferOnData
        def _set_headers(self):
            self.send_response(200)
            self.send_header('Content-type', 'application/json')
            self.end_headers()

        def do_GET(self):
            self._set_headers()
            msg = "<html><body><h1>Get!</h1></body></html>".encode('utf-8')
            self.wfile.write(msg)

        def do_HEAD(self):
            self._set_headers()
            
        def do_POST(self):
            l = int (self.headers.get('content-length'))
            r = self.rfile.read(l).decode()
            boards = json.loads(r)
            #print (r,boards)
            b = np.concatenate([mkTensor(bl,wh) for bl,wh in boards],axis=0)
            score,probs = inferOnData(b)
            c = np.concatenate([score,probs],axis=1)
            tl = c.tolist()
            msg = json.dumps(tl).encode('utf-8')
            self._set_headers()
            self.wfile.write(msg)
    return S

def infer(args):
    inferOnData = inferOnDataFn(args)
    foo = msgpack.Unpacker(sys.stdin.buffer)
    for boards in foo:
        b = np.concatenate([mkTensor(bl,wh) for bl,wh in boards],axis=0)
        (scores,probs) = inferOnData(b)
        sl = [i.item() for i in scores]
        sp = [[j.item() for j in i] for i in probs]
        mp = msgpack.pack((sl,sp),sys.stdout.buffer)
        sys.stdout.flush()

def server(args):
    def run(server_class=HTTPServer, handler_class=mkClass(inferOnDataFn(args)), port=80):
        server_address = ('', port)
        httpd = server_class(server_address, handler_class)
        print ('Starting httpd...')
        httpd.serve_forever()
    run()


def testInfer(args):
    sess,saver = load(args)
    r = np.random.rand(1,7,7,2).astype(np.float32)
#    tIn = tf.constant(r,shape=r.shape)
    inferOnData(args)(r)

def trainOnData(args,trainFeed):
    sess,saver = load(args)    
    graph = tf.get_default_graph()

    iFeed = (graph.get_tensor_by_name(inferName+":0")
          , graph.get_tensor_by_name(scoreTargetName+":0")
          , graph.get_tensor_by_name(probsTargetName+":0"))

    batch_size = 1024
    dataset = tf.data.Dataset.from_tensor_slices(trainFeed).shuffle(20000).batch(batch_size).repeat()
    iter = dataset.make_one_shot_iterator()
    n_batches = trainFeed[0].shape[0] // batch_size    
    
    isTrainingVar = graph.get_tensor_by_name(isTrainingName+":0")    
    sess.run(tf.assign(isTrainingVar,True))
    cost = graph.get_tensor_by_name(costName+":0")    
    costScore = graph.get_tensor_by_name(costScoreName+":0")    
    costProb = graph.get_tensor_by_name(costProbName+":0")    
    costReg = graph.get_tensor_by_name(costRegName+":0")    
    
    update_ops = tf.get_collection(tf.GraphKeys.UPDATE_OPS)       
    with tf.control_dependencies(update_ops):
        optimizer = tf.train.GradientDescentOptimizer(args.lr).minimize(cost,name=optimizerName)
    nextElement = iter.get_next()        
    #sess.run(iter.initializer, feed_dict=dict(zip(iFeed,trainFeed)))                
    print ("training...")
    for i in range(10): # epochs
        for j in range (n_batches): 
            batch = sess.run(nextElement)
            fd = dict(zip(iFeed,batch))
            _,lv,cs,cp,cr = sess.run([optimizer,cost,costScore,costProb,costReg],feed_dict=fd)
            print (i," ",j,"/",n_batches," ", np.mean(lv), np.mean(cs), np.mean(cp), np.mean(cr))
        print ("epoch ",i, " ", np.mean(lv), np.mean(cs), np.mean(cp), np.mean(cr))
    #Save the updated params
    saver.save(sess, args.model,write_meta_graph=False)


def trainData(args):
    """reads training file
    returns boards,winner,moves"""
    with open(args.datafile,"rb") as f:
        (boards,scores,moves) = msgpack.unpack(f)  
    sl = len(scores)
    s = np.array([float(i) for i in scores]).astype(np.float32).reshape(sl,1)
    ml = len(moves)
    assert (sl == ml)
    p = np.zeros((ml,49))    
    p[np.arange(ml),moves]=1
    def mkT(bl,wh):
        t1 = np.array(bl).astype(np.float32).reshape(1,7,7,1)
        t2 = np.array(wh).astype(np.float32).reshape(1,7,7,1)
        ret = np.concatenate([t1,t2],axis=3)
        assert (ret.shape == (1,7,7,2))
        return ret
    b = np.concatenate([mkTensor(bl,wh) for bl,wh in boards],axis=0)
    return (b,s,p)
    
def train(args):
    bsp = trainData(args)    
    trainOnData(args,bsp)    
        
def testTrain(args):
    bs = 100
    boards = np.random.rand(bs,7,7,2).astype(np.float32)
    scores = np.random.rand(bs,1).astype(np.float32)
    scores = np.vectorize(lambda x : -1 if (x < 0.5) else 1)(scores)
    probs = np.eye(49)[np.random.choice(49, bs)]
    trainOnData(args,(boards,scores,probs))
    pass    


def showBoard(bs,probs):
    for b,p in zip(bs,probs):
        c = np.concatenate([b,p.reshape(7,7,1)],axis=2)
        assert (c.shape == (7,7,3))
        plt.imshow(c)
        plt.show()




def cmdParser():
    modelPath =     "./model/a1Model"
    datafile = "mctsTrainBII.mp"
    usageExample = '''
example:    
python alpha1.py create 
python alpha1.py server -load=mlmodel 
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
    subCreate.add_argument('-rweight',type=float,default=0.001,help="output probability count")
    subCreate.add_argument('-model',default=modelPath,help="output Model dir")
    subInfer = subParsers.add_parser('server',help='server -h')
    subInfer.add_argument('-model',default=modelPath,help="input Model dir")
    subTrain = subParsers.add_parser('train',help='train -h')
    subTrain.add_argument('-model',default=modelPath,help="input & output Model dir")
    subTrain.add_argument('-datafile',default=datafile,help="input msgpack file")
    subTrain.add_argument('-lr',type=float,default=0.000001,help="learning rate")
    return parserMode
            
def main():
    parser = cmdParser();            
    args = parser.parse_args()
    if args.mode == 'create' :
        createModel(args)
    elif args.mode == 'server' :
        server(args)
    elif args.mode == 'train' :
        train(args)
main()    
    
    
