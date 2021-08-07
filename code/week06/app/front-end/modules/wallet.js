class Wallet {
  constructor(walletNumber, id) {
    // elements (containers)
    this.main = document.querySelector('main');
    this.div = document.createElement('div');
    this.div.setAttribute('label', walletNumber);
    this.article = document.createElement('article');
    this.section1 = document.createElement('section');
    this.section1.setAttribute('class','left');
    this.section2 = document.createElement('section');
    this.section2.setAttribute('class','right');
    this.span = document.createElement('span');
    // elements
    this.walletName = document.createElement('h2');
    this.oracleName = document.createElement('h2');
    this.oracleDescription = document.createElement('h4');
    this.readBtn = document.createElement('button');
    this.offerInput = document.createElement('input');
    this.offerBtn = document.createElement('button');
    this.retrieveBtn = document.createElement('button');
    this.useBtn = document.createElement('button');
    // fill elements
    this.oracleName.textContent = 'Oracle';
    this.oracleDescription.textContent = '- USDA to ADA -';
    this.readBtn.textContent = 'Read Funds';
    this.offerInput.placeholder = 'Offer Amount';
    this.offerBtn.textContent = 'Offer Funds';
    this.retrieveBtn.textContent = 'Retrieve Funds';
    this.useBtn.textContent = 'Use Oracle';
    // configure elements
    this.div.setAttribute('class', 'fade-in1');
    this.div.setAttribute('id', 'wallet');
    // data
    this.key = id;
    //async constructor
    return (async () => {
      this.walletNumber = await walletNumber;
      this.walletName.textContent = `Wallet ${await walletNumber}`;
      await this.fetchUUID().then(async () => {
        if(this.uuid) {
          await this.displayWallet();
        }
      }).catch(e => console.log(e));
      return this;
    })();
  }
  //get UUID
  async fetchUUID(){
    let url = `../../W${this.walletNumber}.cid`; // compose URL
    const textFetch = async () => {
      let response = await fetch(url);
      if(!response.ok) {
        this.hasUUID = false;
        throw Error(`file not found`);
      }
      return await response.text();
    }
    const validateUUID = async (text) => {
      for(let i = 0; i < text.length;) { // check if uuid is valid
        if((((i !== 8 || i !== 13 || i !== 18 || i !== 23) && text[i] !== '-') ||
            ((i === 8 || i === 13 || i === 18 || i === 23) && text[i] === '-')) &&
            i < text.length-1 && text.length === 36) {
          i++;
        } else if (i === text.length-1 && text[i] !== '-'){
          this.hasUUID = true;
          return await text;
        } else {
          this.hasUUID = false;
          throw Error ('not a valid uuid');
        }
      }
    }
    const extractText = async (text) => { // extract uuid from validaton
      console.log(`successfully created Wallet ${this.walletNumber}`);
      this.uuid = await text;
    }
    await textFetch() // deploy fetch
      .then(async text => await validateUUID(text))
      .then(async text => await extractText(text))
      .catch(err => console.log(err));
  }
  // button functions
  readFunds(){
    console.log(`read funds for Wallet ${this.walletNumber}`);
  }
  retrieveFunds(){
    console.log(`retrieve funds for Wallet ${this.walletNumber}`);
  }
  useOracle(){
    console.log(`use oracle for Wallet ${this.walletNumber}`);
  }
  offerFunds(){
    console.log(`offer ${this.offerInput.value} for Wallet ${this.walletNumber}`);
    this.offerInput.value = '';
  }
  //render wallet
  async displayWallet() {
    // render elements
    this.main.appendChild(this.div);
    this.div.appendChild(this.walletName);
    this.div.appendChild(this.article);
    this.article.appendChild(this.section1);
    this.article.appendChild(this.section2);
    // section1 render
    this.section1.appendChild(this.span);
    this.span.appendChild(this.oracleName);
    this.span.appendChild(this.oracleDescription);
    this.section1.appendChild(this.offerInput);
    this.section1.appendChild(this.offerBtn);
    // section2 render
    this.section2.appendChild(this.readBtn);
    this.section2.appendChild(this.retrieveBtn);
    this.section2.appendChild(this.useBtn);
    //buttons
    this.readBtn.addEventListener('click', () => this.readFunds());
    this.offerBtn.addEventListener('click', () => this.offerFunds());
    this.retrieveBtn.addEventListener('click', () => this.retrieveFunds());
    this.useBtn.addEventListener('click', () => this.useOracle());
  }
}

export default Wallet;
