<template>

  <div>
    <div class="zhanji" v-show="isShowZhanji">
      <div class="title" flex="main:center cross:center">
        实时战况
      </div>

      <div class="con">
        <div class="status" flex="main:center cross:center box:mean">
          <div>昵称</div>
          <div>输赢</div>
        </div>
        <div class="user">
          <div class="item" v-for="n in record" flex="main:center cross:center box:mean">
            <div class="name ">
              <img :src="n.playerId | idToImg(members)" alt="">
              {{n.playerId | idToName(members)}}
            </div>

            <div :class="{'point':true,'fail':!n.flag}">
              {{(!n.flag ? '-' : '+') + n.score}}
            </div>

          </div>
        </div>
      </div>
      <div class="vis" flex="main:center cross:center">
        看客
      </div>
      <div class="vistor">
        <div class="item" flex="main:center cross:center dir:top" v-for="n1 in visitors">
          <img :src="n1.iconUrl" alt="">
          <span>{{n1.playerName}}</span>
        </div>
      </div>

    </div>
    <div class="bg" v-show="isShowZhanji" @click="hideZhanji">

    </div>
  </div>

</template>

<script>
  import {mapGetters} from 'vuex'

  export default {
    name: 'zhanji',

    data() {
      return {}
    },
    mounted() {


    },
    methods: {
      hideZhanji() {
        this.$store.dispatch('showZj', false)
      }
    },
    computed: {
      ...mapGetters({
        'isShowZhanji': 'isShowZhanji',
        'record': 'record',
        'members': 'members',
      }),
      visitors(){
        let user = this.members.find(v=>v.position==0)

        return user
      }

    },
    filters:{
      idToName(val,members){
        let user = members.find(v=>v.PlayerId == val)
        if(user){
          return user.playerName
        }
      },
      idToImg(val,members){
/*        console.log(val,members)*/
        let user = members.find(v=>v.PlayerId == val)
        if(user){
          return user.iconUrl

        }

      }
    }
  }

</script>

<style scoped lang="scss">
  .animated {
    animation-duration: .3s;
  }

  .bg {
    position: fixed;
    top: 0;
    left: 0;
    width: 100%;
    height: 100%;
    z-index: 999;
    border: 1px solid #000;
  }

  .zhanji {
    position: fixed;
    bottom: 0;
    right: -3.8rem;
    width: 3.8rem;
    height: 6.6rem;
    -webkit-transform-origin: 0 100%;
    -moz-transform-origin: 0 100%;
    -ms-transform-origin: 0 100%;
    -o-transform-origin: 0 100%;
    transform-origin: 0 100%;
    -webkit-transform: rotate(-90deg);
    -moz-transform: rotate(-90deg);
    -ms-transform: rotate(-90deg);
    -o-transform: rotate(-90deg);
    transform: rotate(-90deg);
    background: #00302a;
    z-index: 100000000;
    font-size: .24rem;
    box-shadow: 0 0 10px rgba(0, 0, 0, .4);
    .title {
      background: #0b4e40;
      height: .54rem;
      font-size: .28rem;
    }
    .con {
      .status {
        height: .5rem;
        margin-top: .16rem;
        background: #0b4e40;
        font-size: .26rem;
      }
    }
    .user {
      .item {
        padding: .2rem 0;
        height: .8rem;
        img{
          width: .3rem;
          height: .3rem;
          border-radius: .15rem;
          margin-bottom: -2px;
        }
        .point {
          color: #3cec9b;
          font-size: .32rem;

          &.fail {
            color: #ff0000;
          }
        }
      }
    }
    .vis {
      height: .56rem;
      background: #0b4e40;
      font-size: .28rem;
      margin-top: .2rem;
    }
    .vistor {
      overflow: auto;
      height: 1.5rem;
      .item {
        float: left;
        margin: .16rem;
        img {
          width: .4rem;
          height: .4rem;
          border-radius: .4rem;
          margin-bottom: .1rem;
        }
        span {
          font-size: .2rem;
        }
      }
    }

  }


</style>
