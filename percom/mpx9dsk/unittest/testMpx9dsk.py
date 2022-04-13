'''
Created on Apr 20, 2017

@author: james
'''
import unittest
import mpx9dsk

class Test(unittest.TestCase):


    def setUp(self):
        self.dskimg = mpx9dsk.Mpx9DskImg('mpx9dsk1.dsk')


    def tearDown(self):
        pass


    def testDisplayDir(self):
        self.dskimg.displayDir(False)


if __name__ == "__main__":
    #import sys;sys.argv = ['', 'Test.testName']
    unittest.main()
    
    
    
# python -m unittest test_module1 test_module2
# python -m unittest test_module.TestClass
# python -m unittest test_module.TestClass.test_method

# #import usertest
# #import configtest # first test
# import unittest   # second test
# 
# class ConfigTestCase(unittest.TestCase):
#     def setUp(self):
#     print 'stp'
#         ##set up code
# 
#     def runTest(self):
# 
#         #runs test
#     print 'stp'
# 
# def suite():
#     """
#         Gather all the tests from this module in a test suite.
#     """
#     test_suite = unittest.TestSuite() # class unittest.TestSuite(tests=())
#     test_suite.addTest(unittest.makeSuite(ConfigTestCase))
#     return test_suite
# 
# mySuit=suite()
# 
# 
# runner=unittest.TextTestRunner()
# runner.run(mySuit)