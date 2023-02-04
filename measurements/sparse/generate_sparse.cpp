#include <iostream>
#include <vector>
#include <cstdlib>

using namespace std;

const int max_value = 1000;
const int min_value = -500;

int main(int argc, char** argv){
	int n,k,m;
	float sparsityA = 1,sparsityB  = 1;

	if(argc > 1){
		n = atoi(argv[1]);
	}
	else cin >> n;
	k = m = n;
	//NxK * KxM szorzas
	//cin >> n >> k >> m >> sparsityA >> sparsityB;
	vector<vector<int>> A(n,vector<int>(k,0));
	vector<vector<int>> B(k,vector<int>(m,0));

	srand (time(NULL));


	for(int i = 0; i<(float)n*k*sparsityA; ++i){
		int sor,oszlop;
		do {
			sor = rand() % n;
			oszlop = rand() % k;
		}while(A[sor][oszlop] != 0);
		do{
			A[sor][oszlop] =  rand() % (max_value - min_value) + min_value;
		}while(A[sor][oszlop] == 0);
	}
	for(int i = 0; i<(float)k*m*sparsityB; ++i){
		int sor,oszlop;
		do {
			sor = rand() % k;
			oszlop = rand() % m;
		}while(B[sor][oszlop] != 0);
		do{
			B[sor][oszlop] =  rand() % (max_value - min_value) + min_value;
		}while(B[sor][oszlop] == 0);
	}

	// for(int i = 0; i<n; ++i){
	// 	for(int j = 0; j<k; ++j){
	// 		cout << A[i][j] << " ";
	// 	}
	// 	cout << endl;
	// }
	// cout << endl;
	// for(int i = 0; i<k; ++i){
	// 	for(int j = 0; j<m; ++j){
	// 		cout << B[i][j] << " ";
	// 	}
	// 	cout << endl;
	// }
	cout << "[";
	bool q = false;
	for(int i = 0; i<n; ++i){
		bool b = false;
		for(int j = 0; j<k; ++j){
			if(A[i][j] != 0){
				if(!b){
					b = true;
					if(q) cout << ",";
					cout << "{" << i+1 << ",[";
				}
				else cout << ",";
				cout << "{" << j+1 << "," << A[i][j] << "}";
			}
		}
		if(b){
			q = true;
			cout << "]}";
		}
	}
	cout << "],[";
	q = false;
	for(int i = 0; i<m; ++i){
		bool b = false;
		for(int j = 0; j<k; ++j){
			if(B[j][i] != 0){
				if(!b){
					b = true;
					if(q) cout << ",";
					cout << "{" << i+1 << ",[";
				}
				else cout << ",";
				cout << "{" << j+1 << "," << B[j][i] << "}";
			}
		}
		if(b){
			q = true;
			cout << "]}";
		}
	}
	cout << "]";


	return 0;
}