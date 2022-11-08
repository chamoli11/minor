#include <iostream>
#include <vector>
#include <string>
#include <fstream>
#include <cmath>
#include <map>
using namespace std;

class Table {
	public:
		vector<string> attrName;/* stores attribute name*/
		vector<vector<string> > data;/* store the values of table*/

		vector<vector<string> > attrValueList;/* store data in transpose form*/
		void extractAttrValue() {
			attrValueList.resize(attrName.size());/* reshape on basis of column*/
			for(int j=0; j<attrName.size(); j++) {/* nested for loop for pushing the column data value
			into rows on basis of attribute size */

				map<string, int> value;
				for(int i=0; i<data.size(); i++) {
					value[data[i][j]]=1;
				}

				for(auto iter=value.begin(); iter != value.end(); iter++) {
					attrValueList[j].push_back(iter->first);
				}
			}
		}
};

class Node {
	public:
		int criteriaAttrIndex;
		string attrValue;

		int treeIndex;
		bool isLeaf;
		string label;

		vector<int > children;

		Node() {
			isLeaf = false;
		}
};

class DecisionTree {
	public:
		Table initialTable;
		
		vector<Node> tree;

		DecisionTree(Table table) {
			initialTable = table;
			initialTable.extractAttrValue();

			Node root;
			root.treeIndex=0;
			tree.push_back(root);
			run(initialTable, 0);
			printTree(0, "");

			cout<< "<-- finish generating decision tree -->" << endl << endl;
		}

		string guess(vector<string> row) {
			string label = "";
			int leafNode = dfs(row, 0);
			if(leafNode == -1) {
				return "dfs failed";
			}
			label = tree[leafNode].label;
			return label;
		}

		int dfs(vector<string>& row, int here) {
			if(tree[here].isLeaf) {
				return here;
			}

			int criteriaAttrIndex = tree[here].criteriaAttrIndex;

			for(int i=0;i<tree[here].children.size(); i++) {
				int next = tree[here].children[i];

				if (row[criteriaAttrIndex] == tree[next].attrValue) {
					return dfs(row, next);
				}
			}
			return -1;
		}

		void run(Table table, int nodeIndex) {
			/* we are firstly seeing that whether class label is all same or not if not then we find attribute row of max gain ratio then we stotes that
			rows index, after that we check whether 80 percent value of class label is same or not , if it is we declare it as leaf node othewise we start
			make a tree based on selected attribute after that we do same recurrsion on next tables */
			if(isLeafNode(table) == true) {
				tree[nodeIndex].isLeaf = true;
				tree[nodeIndex].label = table.data.back().back();/*[i][j]*/
				return;
			}

			int selectedAttrIndex = getSelectedAttribute(table);/* return index of table dividing
			attribute index*/

			map<string, vector<int> > attrValueMap;
			for(int i=0;i<table.data.size();i++) {
				attrValueMap[table.data[i][selectedAttrIndex]].push_back(i);
			}

			tree[nodeIndex].criteriaAttrIndex = selectedAttrIndex;/* */

			pair<string, int> majority = getMajorityLabel(table);
			if((double)majority.second/table.data.size() > 0.8) {
				tree[nodeIndex].isLeaf = true;
				tree[nodeIndex].label = majority.first;
				return;
			}
			vector<string> types;
			
			for(int i=0;i< initialTable.attrValueList[selectedAttrIndex].size(); i++) {
				string attrValue = initialTable.attrValueList[selectedAttrIndex][i];
				bool flag= false;
				
				for (int i=0; i<types.size(); i++){
					int value = strcmp(types[i],attrValue);  
   					if(value == 0) {
						flag=true;
					}
				}
				if(flag== false){		
					Table nextTable;
					vector<int> candi = attrValueMap[attrValue];
					for(int i=0;i<candi.size(); i++) {
						nextTable.data.push_back(table.data[candi[i]]);
					}

					Node nextNode;
					nextNode.attrValue = attrValue;
					nextNode.treeIndex = (int)tree.size();
					tree[nodeIndex].children.push_back(nextNode.treeIndex);
					tree.push_back(nextNode);

					// for empty table
					if(nextTable.data.size()==0) {
						nextNode.isLeaf = true;
						nextNode.label = getMajorityLabel(table).first;
						tree[nextNode.treeIndex] = nextNode;
					} else {
						run(nextTable, nextNode.treeIndex);
					}
				}
			}
		}

		double getEstimatedError(double f, int N) {
			double z = 0.69;
			if(N==0) {
				cout << ":: getEstimatedError :: N is zero" << endl;
				exit(0);
			}
			return (f+z*z/(2*N)+z*sqrt(f/N-f*f/N+z*z/(4*N*N)))/(1+z*z/N);
		}

		pair<string, int> getMajorityLabel(Table table) {
			string majorLabel = "";
			int majorCount = 0;

			map<string, int> labelCount;
			for(int i=0;i< table.data.size(); i++) {
				labelCount[table.data[i].back()]++;

				if(labelCount[table.data[i].back()] > majorCount) {
					majorCount = labelCount[table.data[i].back()];
					majorLabel = table.data[i].back();
				}
			}

			return {majorLabel, majorCount};
		}


		bool isLeafNode(Table table) {
			/* checks if first data value is equla to all the data values of class 
			and if fucntion returns true claiming it is leaf node*/
			for(int i=1;i < table.data.size();i++) {
				if(table.data[0].back() != table.data[i].back()) {
					return false;
				}
			}
			return true;
		}

		int getSelectedAttribute(Table table) {/*returns best attribute on basis of Gain Ratio 
		and Information Gain*/
			int maxAttrIndex = -1;
			double maxAttrValue = 0.0;

			// except label
			for(int i=0; i< initialTable.attrName.size()-1; i++) {
				if(maxAttrValue < getGainRatio(table, i)) {
					maxAttrValue = getGainRatio(table, i);
					maxAttrIndex = i;
				}
			}

			return maxAttrIndex;
		}

		double getGainRatio(Table table, int attrIndex) {
			return getGain(table, attrIndex)/getSplitInfoAttrD(table, attrIndex);
		}

		double getInfoD(Table table) {
			double ret = 0.0;

			int itemCount = (int)table.data.size();/* gives table data length( no of rows)*/
			map<string, int> labelCount;

			for(int i=0;i<table.data.size();i++) {/*counts the number of distinct attributes class label
			for ex-  data size =14, number of y=9, number of n=5*/
				labelCount[table.data[i].back()]++;
			}

			for(auto iter=labelCount.begin(); iter != labelCount.end(); iter++) {
				/*iter = distinct number of class labels*/
				/*
				for ex- y,n 
				iter[0]= gives entropy of y
				iter[1]= gives entropy of n*/
				
				double p = (double)iter->second/itemCount;

				ret += -1.0 * p * log(p)/log(2);
			}

			return ret;
		}

		double getInfoAttrD(Table table, int attrIndex) {
			double ret = 0.0;
			int itemCount = (int)table.data.size();

			map<string, vector<int> > attrValueMap;
			/*stores all the indexes of row where the selected attribute is present*/
			

			for(int i=0;i<table.data.size();i++) {

				attrValueMap[table.data[i][attrIndex]].push_back(i);
			}

			for(auto iter=attrValueMap.begin(); iter != attrValueMap.end(); iter++) {
				Table nextTable;
				for(int i=0;i<iter->second.size(); i++) {
					/*it pushes the whole row where the selected attribute is present to a new table*/
					nextTable.data.push_back(table.data[iter->second[i]]);
				}
				int nextItemCount = (int)nextTable.data.size(); /*next item count= 5*/

				ret += (double)nextItemCount/itemCount * getInfoD(nextTable);
			}

			return ret;
		}

		double getGain(Table table, int attrIndex) {
			/*gain = info (d) - info a(d)*/
			return getInfoD(table)-getInfoAttrD(table, attrIndex);
		}

		double getSplitInfoAttrD(Table table, int attrIndex) {
			double ret = 0.0;

			int itemCount = (int)table.data.size();

			map<string, vector<int> > attrValueMap;
			for(int i=0;i<table.data.size();i++) {
				attrValueMap[table.data[i][attrIndex]].push_back(i);
			}

			for(auto iter=attrValueMap.begin(); iter != attrValueMap.end(); iter++) {
				Table nextTable;
				for(int i=0;i<iter->second.size(); i++) {
					nextTable.data.push_back(table.data[iter->second[i]]);
				}
				int nextItemCount = (int)nextTable.data.size();

				double d = (double)nextItemCount/itemCount;
				ret += -1.0 * d * log(d) / log(2);
			}

			return ret;
		}

		/*
		 * Enumerates through all the nodes of the tree and prints all the branches 
		 */
		void printTree(int nodeIndex, string branch) {
			if (tree[nodeIndex].isLeaf == true)
				cout << branch << "Label: " << tree[nodeIndex].label << "\n";

			for(int i = 0; i < tree[nodeIndex].children.size(); i++) {
				int childIndex = tree[nodeIndex].children[i];

				string attributeName = initialTable.attrName[tree[nodeIndex].criteriaAttrIndex];
				string attributeValue = tree[childIndex].attrValue;

				printTree(childIndex, branch + attributeName + " = " + attributeValue + ", ");
			}
		}
};


class InputReader {
	private:
		ifstream fin;
		Table table;
	public:
		InputReader(string filename) {
			/*using input stream class object to open the file*/
			fin.open(filename);
			if(!fin) {
				cout << filename << " file could not be opened\n";
				exit(0);
			}
			parse();
		}
		void parse() {/* read the data from file */
			string str;
			bool isAttrName = true;
			while(!getline(fin, str).eof()){/*line by line data is passed in str variable */
				vector<string> row;
				/* extracting the words from line and storing the whole line in row varible*/
				int pre = 0;
				for(int i=0;i<str.size();i++){
					if(str[i] == '\t') {
						string col = str.substr(pre, i-pre);

						row.push_back(col);
						pre = i+1;
					}
				}
				string col = str.substr(pre, str.size()-pre-1);
				row.push_back(col);

				if(isAttrName) {/* storing atrribute name*/
					table.attrName = row;
					isAttrName = false;
				} else {/* storing the value of table*/
					table.data.push_back(row);
				}
			}
		}
		Table getTable() {
			return table;
		}
};

class OutputPrinter {
	private:
		ofstream fout;
	public:
		OutputPrinter(string filename) {
			fout.open(filename);
			if(!fout) {
				cout << filename << " file could not be opened\n";
				exit(0);
			}
		}

		string joinByTab(vector<string> row) {
			string ret = "";
			for(int i=0; i< row.size(); i++) {
				ret += row[i];
				if(i != row.size() -1) {
					ret += '\t';
				}
			}
			return ret;
		}

		void addLine(string str) {
			fout << str << endl;
		}
};

int main(int argc, const char * argv[]) {


	// we have taken 3 input values , so argc value will be 4 
	if(argc!=4) {
		cout << "Please follow this format. dt.exe [train.txt] [test.txt] [result.txt]";
		return 0;
	}


	// train dataset is passed here
	string trainFileName = argv[1];
	InputReader trainInputReader(trainFileName);/*training dataset in passed in object of inputreader  class*/
	DecisionTree decisionTree(trainInputReader.getTable());/* taking the genrated the table and passing it into object og decision tree class*/

	// string testFileName = argv[2];
	// InputReader testInputReader(testFileName);
	// Table test = testInputReader.getTable();

	// string resultFileName = argv[3];
	// OutputPrinter outputPrinter(resultFileName);
	// outputPrinter.addLine(outputPrinter.joinByTab(test.attrName));
	// for(int i=0;i < test.data.size(); i++) {
	// 	vector<string> result = test.data[i];
	// 	result.push_back(decisionTree.guess(test.data[i]));
	// 	outputPrinter.addLine(outputPrinter.joinByTab(result));
	// }

	/* for answer check */
	/*
	   InputReader answerInputReader("dt_answer1.txt");
	   Table answer = answerInputReader.getTable();
	   int totalCount = (int)answer.data.size();
	   int hitCount = 0;
	   for(int i=0;i < test.data.size(); i++) {
	   if(answer.data[i].back() == decisionTree.guess(test.data[i])) {
	   hitCount++;
	   }
	   }
	   cout << "Accuracy: " << (double)hitCount/totalCount*100 << "%";
	   cout << "(" << hitCount << "/" << totalCount << ")" << endl;
	   */
	return 0;
}
