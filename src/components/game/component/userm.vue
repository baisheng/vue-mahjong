<template>

  <div v-show="userChose.show">
    <div class="container">
      <div class="head">
        <img :src="userChose.data.iconUrl" alt="">
      </div>
      <p>{{userChose.data.playerName}}</p>
      <div class="con">
        <p class="all">牌局数</p>
        <strong class="all_t">{{userChose.data.totalCount}}</strong>
        <p class="de">胜/平</p>
        <strong
          class="de_t">{{userChose.data.winCount}}/{{userChose.data.totalCount - userChose.data.winCount}}</strong>
      </div>
      <div class="close" @click="close">
        <img src="../../../assets/new/back.png" alt="">
      </div>
      <div class="bottom" v-show="isCan" >
        <div class="item" flex="main:center cross:center box:mean">
          <a href="javascript:;" v-for="n in tools" @click='sendMsg(n.num2,n.num)'>
            <div class="icon">
              <img :src="n.img" alt="">
            </div>
            <div class="price">
              <img src="../../../assets/icon/emoji/dia.png" alt="">
              {{n.num}}
            </div>
          </a>
        </div>
        <div class="item" flex="main:center cross:center box:mean">
          <a href="javascript:;" v-for="n in tools2" @click='sendMsg(n.num2,n.num)'>
            <div class="icon">
              <img :src="n.img" alt="">
            </div>
            <div class="price">
              <img src="../../../assets/icon/emoji/dia.png" alt="">
              {{n.num}}
            </div>
          </a>
        </div>

      </div>
    </div>
    <div class="bg"></div>


  </div>
</template>


<script>
  import {mapGetters} from 'vuex'

  export default {
    data() {
      return {
        tools: [
          {
            img: require('../../../assets/icon/emoji/1/1.png'),
            img2: require('../../../assets/icon/emoji/2/1.png'),
            num: 2,
            num2:1
          }, {
            img: require('../../../assets/icon/emoji/1/2.png'),
            img2: require('../../../assets/icon/emoji/2/2.png'),
            num: 2,
            num2:2
          },
          {
            img: require('../../../assets/icon/emoji/1/3.png'),
            img2: require('../../../assets/icon/emoji/2/3.png'),
            num: 5,
            num2:3
          },
          {
            img: require('../../../assets/icon/emoji/1/4.png'),
            img2: require('../../../assets/icon/emoji/2/4.png'),
            num: 2,
            num2:4
          },
          {
            img: require('../../../assets/icon/emoji/1/5.png'),
            img2: require('../../../assets/icon/emoji/2/5.png'),
            num: 10,
            num2:5
          },


        ],

        tools2: [
          {
            img: require('../../../assets/icon/emoji/1/6.png'),
            img2: require('../../../assets/icon/emoji/2/6.png'),
            num: 5,
            num2:6
          },
          {
            img: require('../../../assets/icon/emoji/1/7.png'),
            img2: require('../../../assets/icon/emoji/2/7.png'),
            num: 1,
            num2:7
          },
          {
            img: require('../../../assets/icon/emoji/1/8.png'),
            img2: require('../../../assets/icon/emoji/2/8.png'),
            num: 10,
            num2:8
          },
          {
            img: require('../../../assets/icon/emoji/1/9.png'),
            img2: require('../../../assets/icon/emoji/2/9.png'),
            num: 10,
            num2:9
          }, {
            img: require('../../../assets/icon/emoji/1/10.png'),
            img2: require('../../../assets/icon/emoji/2/10.png'),
            num: 1,
            num2:10
          }
        ]
      }
    },
    mounted() {


    },
    created() {

    },
    computed: {
      ...mapGetters({
        'members': 'members',
        'userChose': 'userChose',
        'user_info': 'user_info',
        'members': 'members',
      }),
      isCan(){
        let id = this.user_info.id;
        let me = this.members.find(v => v.PlayerId == id)
        return me.position == 0 ? false : true
      }
    },
    methods: {
      close() {
        this.$store.dispatch('showUserModal', {show: false})
      },
      sendMsg(id,num){

          if(num>this.user_info.diamond){
              return alert('余额不足')
          }

        this.socket.send(508, {
          type: 2,
          content: id + '',
          toid: this.userChose.data.PlayerId
        })
      }
    }
  }
</script>

<style scoped lang="scss">
  .container {
    position: fixed;
    top: 50%;
    left: 50%;
    -webkit-transform: translate(-50%, -50%) rotate(-90deg);
    -moz-transform: translate(-50%, -50%) rotate(-90deg);
    -ms-transform: translate(-50%, -50%) rotate(-90deg);
    -o-transform: translate(-50%, -50%) rotate(-90deg);
    transform: translate(-50%, -50%) rotate(-90deg);
    width: 3.58rem;
    height: 4.34rem;
    background: rgba(2, 64, 54, .5);
    border-radius: .2rem;
    text-align: center;
    z-index: 1000000000;
    .bottom {
      position: absolute;
      bottom: 0;
      left: 0;
      width: 100%;
      height: 1.4rem;
      background: rgba(2, 64, 54, .7);
      border-radius: 0 0 .2rem .2rem;
      .item {
        width: 100%;
        height: .6rem;
        margin: .05rem 0;
        a {
          padding: .01rem;
          text-decoration: none;
        }
        .icon {
          img {
            width: .41rem;
          }
        }
        .price {
          font-size: .18rem;
          color: #008800;
          text-decoration: none;
          img {
            width: .2rem;
          }
        }
      }
    }
    .close {
      position: absolute;
      top: -.3rem;
      right: -.3rem;
      width: .6rem;
      height: .6rem;
      img {
        width: 100%;
      }
    }
    p {
      line-height: 1;
      margin-top: .2rem;
      font-size: .2rem;
    }
    .head {
      position: absolute;
      top: .2rem;
      left: .3rem;
      width: .4rem;
      height: .4rem;
      border-radius: .2rem;
      overflow: hidden;
      img {
        width: 100%;
        height: 100%;

      }
    }
    .con {
      margin: .1rem auto;
      width: 2.16rem;
      height: 2.16rem;
      border: .14rem solid #08a679;
      border-radius: 1.08rem;
      background: rgba(2, 64, 54, 1);
      .all {
        margin-top: .2rem;
        color: #68e1d7;
        font-weight: 200;
        font-size: .24rem;
      }
      .all_t {
        font-weight: 200;
        font-style: normal;
        font-size: .3rem;
      }
      .de {
        margin-top: .14rem;
        color: #68e1d7;
        font-weight: 200;
        font-size: .24rem;
        line-height: 1;

      }
      .de_t {
        font-style: normal;
        font-weight: 200;
        font-size: .2rem;
        line-height: 1;
      }
    }
  }
</style>
