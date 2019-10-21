#include<iostream>
#include<vector>
#include<algorithm>
#include<math.h>
using namespace std;

const int64_t QUERY_SIZE = 1e5 + 7;
const int64_t MAX_BOUND = (1LL<<37);
const int64_t MAX_SEGMENT = sqrtl(MAX_BOUND) + 1;
const int64_t CACHE_SIZE = 32768;

int32_t shift[] = {1, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47, 49, 53, 59}; // 16

int32_t cycle[] = {2, 4};

int64_t query[QUERY_SIZE];

vector <pair<int64_t, int64_t>> queries;

vector <int64_t> primes;
vector <int64_t> multiples;
vector <char> sieve;

vector <int64_t> firstY, secondY, thirdY;
vector <int64_t> firstIndex, secondIndex, thirdIndex;

void initPrimes(int64_t limit){
	for(int64_t i = 0; i * 60 <= limit; i++){
		for(int64_t j = 0; j < 16; j++){
			int64_t k = i * 60 + shift[j];

			if (k > limit){
				continue;
			}

			if (sieve[k]){
				primes.push_back(k);
				multiples.push_back(k * k);

				int64_t tmp = k * k;

				for(int64_t l = tmp; l <= limit; l += tmp){
					sieve[l] = false;
				}
			}
		}
	}
}

void runSieve(int32_t segment_size){
	for (size_t i = 0; i < primes.size(); i++){
      	int64_t j = multiples[i];

      	for (int64_t k = primes[i] * primes[i] * 2; j < segment_size; j += k){
        	sieve[j] = false;
      	}

      	multiples[i] = j - segment_size;
    }
}

void firstLoop(int64_t l, int64_t r){
	int64_t xUpper = sqrtl(r / 4) + 1;
	int64_t x = 1, y;
	int64_t k1 = 0, k = 0;

	while (x < xUpper){
		k1 = 4 * x * x;

		y = firstY[x];
		int32_t index = firstIndex[x];

		if (x % 3 == 0){
			while (true){
				k = k1 + y * y;

				int64_t tmp = k - l;

				if (tmp < 0){
					y += cycle[(++index & 1)];
					continue;
				}

				if (k > r){
					break;
				}

				sieve[tmp] = !sieve[tmp];
				y += cycle[(++index & 1)];
			}
		}else{
			while (true){
				k = k1 + y * y;

				int64_t tmp = k - l;

				if (tmp < 0){
					y += 2;
					continue;
				}

				if (k > r){
					break;
				}

				sieve[tmp] = !sieve[tmp];
				y += 2;
			}
		}

		firstIndex[x] = index;
		firstY[x] = y;
		x++;
	}
}

void secondLoop(int64_t l, int64_t r){
	int64_t xUpper = sqrtl(r / 3) + 1;
	int64_t x = 1, y = 0;
	int64_t k1 = 0, k = 0;

	while (x < xUpper){
		k1 = 3 * x * x;

		y = secondY[x];
		int32_t index = secondIndex[x]; 

		while (true){
			k = k1 + y * y;

			int64_t tmp = k - l;

			if (tmp < 0){
				y += cycle[(++index & 1)];
				continue;
			}

			if (k > r){
				break;
			}

			sieve[tmp] = !sieve[tmp];
			y += cycle[(++index & 1)];
		}

		secondY[x] = y;
		secondIndex[x] = index;
		x += 2;
	}
}

void initY(int64_t x, int64_t l, int64_t r){
	thirdY[x] = min(x, (int64_t)sqrt(x*x*3 - l));

	if ((x & 1) == 0){
		thirdIndex[x] = 1;
		
		while(thirdY[x] % 6 != 1){
			thirdY[x]++;
		}
	}else{
		thirdIndex[x] = 0;

		while(thirdY[x] % 6 != 2){
			thirdY[x]++;
		}
	}
}	

void thirdLoop(int64_t l, int64_t r){
	int64_t xUpper = sqrtl(r / 2) + 1;
	int64_t x = sqrtl(l / 3), y = 0;
	int64_t k1 = 0, k = 0;

	while (x < xUpper) {
		int32_t index = 0;

		k1 = 3 * x * x;

		if (thirdY[x] == MAX_BOUND){
			initY(x, l, r);
		}

		y = thirdY[x];
		index = thirdIndex[x];

		while (y > 0){
			k = k1 - y * y;

			if (k > r)
				break;

			if (k >= l && k <= r && y < x) {
				sieve[k - l] = !sieve[k - l];
			}

			y -= cycle[(++index & 1)];
		}

		thirdY[x] = y;
		thirdIndex[x] = index;

		x++;
	}	
}

void sieveOfAtkin(){
	int64_t upper_bound, n;

	cin >> upper_bound;

	cin >> n;

	for(int i = 1; i <= n; i++){
		cin >> query[i];
		queries.emplace_back(query[i], i);
	}

	sort(queries.begin(), queries.end());

	size_t queIdx = 0;

	while(queIdx < queries.size()){
		if (queries[queIdx].first <= 1){
			query[queries[queIdx].second] = 0;
			queIdx++;
			continue;
		}

		if (queries[queIdx].first <= 2){
			query[queries[queIdx].second] = 1;
			queIdx++;
			continue;
		}

		if (queries[queIdx].first <= 4){
			query[queries[queIdx].second] = 2;
			queIdx++;
			continue;
		}

		if (queries[queIdx].first <= 5){
			query[queries[queIdx].second] = 3;
			queIdx++;
			continue;
		}

		break;
	}

	int64_t square_root = sqrtl(upper_bound) + 1; 
	int64_t segment_size = max(square_root, CACHE_SIZE);

	sieve = vector <char> (segment_size);

	firstY = vector <int64_t> (square_root + 1);
	std::fill(firstY.begin(), firstY.end(), 1);

	firstIndex = vector <int64_t> (square_root + 1);
	std::fill(firstIndex.begin(), firstIndex.end(), 0);

	secondY = vector <int64_t> (square_root + 1);
	std::fill(secondY.begin(), secondY.end(), 2);

	secondIndex = vector <int64_t> (square_root + 1);
	std::fill(secondIndex.begin(), secondIndex.end(), 1);

	thirdY = vector <int64_t> (square_root + 1);
	std::fill(thirdY.begin(), thirdY.end(), MAX_BOUND);

	thirdIndex = vector <int64_t> (square_root + 1);
	std::fill(thirdIndex.begin(), thirdIndex.end(), 0);

	std::fill(sieve.begin(), sieve.end(), false);

	int64_t count = 3;

	int64_t mOuter = 0, mInner = 0;

	for (int64_t l = 0; l <= upper_bound; l += segment_size){
		std::fill(sieve.begin(), sieve.end(), false);

	    int64_t r = l + segment_size - 1;
	    r = min(r, upper_bound);

	    firstLoop(l, r);
		secondLoop(l, r);
		thirdLoop(l, r);

		if (l == 0){
			initPrimes(segment_size);
		}

		runSieve(segment_size);

		while(true){
			int64_t k = mOuter * 60 + shift[mInner];

			if (k <= r){
				while(queIdx < queries.size()){
					if (queries[queIdx].first < k){
						query[queries[queIdx].second] = count;
						queIdx++;
						continue;
					}

					break;
				}

				count += (sieve[k - l]);

				while(queIdx < queries.size()){
					if (queries[queIdx].first <= k){
						query[queries[queIdx].second] = count;
						queIdx++;
						continue;
					}

					break;
				}

				mInner = mInner + 1;

				if (mInner == 16){
					mInner = 0;
					mOuter += 1;
				}
			}else{
				break;
			}
		}
	}

	while(queIdx < queries.size()){
		query[queries[queIdx].second] = count;
		queIdx++;
	}

	for(int i = 1; i <= n; i++){
		cout << query[i] << endl;
	}
}

int32_t main()
{
	ios_base::sync_with_stdio(false);
	cin.tie(0);
	cout.tie(0);

	sieveOfAtkin();

    return 0;
}
