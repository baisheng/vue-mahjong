<template>
  <div class="rankD" v-if="roomDetail.info">

    <!--{{owner}}-->
    <div class="head" flex="main:center cross:center box:last">
      <p>房号：{{roomDetail.info.roomId}} 象山麻将 已结束</p>
      <!--<a href="javascript:;">建议反馈</a>-->
    </div>
    <div class="userinfo" flex="main:center cross:center box:first" v-if="roomDetail">
      <div class="img">
        <img :src="owner.icon" alt="">
      </div>
      <div class="info">
        &nbsp;&nbsp;&nbsp;&nbsp;房主（{{owner.name}}）
      </div>
      <div class="time">
        {{roomDetail.info.startTime | formatMDHS}} 创局
      </div>
    </div>

    <div class="game_time" flex="main:center cross:center">
      牌局时长：{{roomDetail.info.totalTime | formatS}}分钟
    </div>

    <div class="two" flex="main:center cross:center box:mean" v-if="topMembers.length">
      <div class="p1" flex=" cross:center">
        (王者){{topMembers[topMembers.length - 1].name}}
        <img :src="topMembers[topMembers.length-1].icon" alt="">
      </div>
      <div class="p1" flex=" cross:center">
        <img :src="topMembers[0].icon" alt=""> {{topMembers[0].name}}(浪货)
      </div>
    </div>
    <div class="user_list" flex="main:center cross:center dir:top box:mean" >

      <template v-if="roomDetail.members.length">
        <div class="item" v-for="n in members.data" flex=" cross:center box:justify" >
          <img :src="n.icon" alt="">
          <span>{{n.name}}</span>
          <span><b v-show="n.winFlag==0"></b>{{n.score}}</span>
        </div>
      </template>


      <div class="tip" v-if="!members.isHavaMembers">
        房间没人进入
      </div>

    </div>


  </div>
</template>

<script>
  import {mapGetters} from 'vuex'
  import brg from '../../utils/middle'

  const moment = require('moment')
  export default {
    data() {
      return {
        isCount: false,

      }
    },
    created() {

      let id = this.$route.params.id;
      this.socket.send(503, {
        recordId: id
      })

    },
    beforeMount() {
      brg.$on('protoMsg', (obj) => {
        let {id, msg} = obj;
        if (id == 504) {
          this.$store.dispatch('fetchRoomRecord2', msg.list[0])
        }
      })
    },
    mounted() {

    },

    filters: {
      formatMD(val) {
        return moment(val * 1000).format('MM-DD')
      },
      formatMDHS(val) {
        return moment(val * 1000).format('MM-DD HH:mm')
      },
      formatHS(val) {
        return moment(val * 1000).format('HH:mm')
      },
      formatS(val) {
        return val / 60
      }

    },
    methods: {
      compare(attr, rev) {
        if (rev == undefined) {
          rev = 1;
        } else {
          rev = (rev) ? 1 : -1;
        }
        return (a, b) => {
          a = a[attr];
          b = b[attr];
          if (a < b) {
            return rev * -1;
          }
          if (a > b) {
            return rev * 1;
          }
          return 0;
        }
      }
    },
    computed: {
      ...mapGetters({
        'rank_record': 'rank_record',
        'user_info': 'user_info',
        'roomDetail': 'one_record',
      }),
      members() {
        let one = this.roomDetail

        if (!this.roomDetail.info) {
          return {}
        }

        if (typeof(one.members) !== 'undefined' && one.members.length > 1) {
          return {
            isHavaMembers: true,
            data: one.members,
          }
        } else {
          return {
            isHavaMembers: false,
            data: []
          }
        }
      },


      owner() {
        if (!this.roomDetail.info.ownerId) {
          return {}
        }
        let id = this.roomDetail.info.ownerId
        let user = this.roomDetail.members.find(v => v.playerId == id)
        if(user){
          return user
        }else{
          return {}
        }

      },
      topMembers() {
        if (!this.members) {
          return []
        }
        if (this.members.isHavaMembers) {
          let new1 = [...this.members.data]
          new1.forEach(v => {
            if (!v.winFlag) {
              v.score = -v.score
            }
          });

          new1 = new1.sort(this.compare('score',1))
          return new1

        } else {
          return []
        }
      }
    }
  }
</script>

<style scoped lang="scss">
  .tip {
    margin: 30px;
    text-align: center;
    color: #333;
  }

  .rankD {
    border: 2px solid #500047;
    margin: .5rem .46rem;
    background: #ffe8bc;
    padding: .24rem;
    border-radius: 8px;
    height: 9.6rem;
    font-size: .24rem;
    padding-bottom: .34rem;
    .head {
      color: #635646;
      margin-bottom: 10px;
      p {
        text-align: left;
        padding-left: .2rem;
        font-weight: 600;
      }
      a {
        text-decoration: none;
        color: #fff;
        background: #e60012;
        display: inline-block;
        padding: .08rem .1rem;
        border-radius: 5px;
        box-shadow: 0 0 10px rgba(0, 0, 0, .3);
      }
    }
    .userinfo {
      height: 1.6rem;
      color: #cfa972;
      border: 2px solid #cfa972;
      background: #6a3906;
      border-radius: 8px;
      box-shadow: 0 0 8px rgba(0, 0, 0, .3) inset;
      .img {

        margin-left: .3rem;
        img {
          width: .9rem;
          height: .9rem;
        }
      }
    }
    .game_time {
      height: .64rem;
      color: #6a3906;
      font-weight: bold;
      font-size: .28rem;
    }
    .two {
      background: #cfa972;
      height: 1.08rem;
      border: 2px solid #cfa972;
      border-radius: 8px;
      box-shadow: 3px 3px 3px rgba(0, 0, 0, .2) inset;
      color: #635646;
      font-weight: 600;
      padding: .2rem;
      img {
        margin: .14rem;
      }
    }
    .user_list {
      box-shadow: 3px 3px 3px rgba(0, 0, 0, .2) inset;
      background: #a6937c;
      border: 2px solid #a6937c;
      height: 4.76rem;
      border-radius: 8px;
      margin-top: .22rem;
      margin-bottom: .34rem;
      .item {
        color: #010101;
        width: 100%;
        padding: 0 .5rem;
        font-weight: 400;
        font-size: .28rem;
        img {
          width: .8rem;
          height: .8rem;
          margin-right: .2rem;
        }
      }
    }
  }

  img {
    width: .8rem;
    height: .8rem;
    border: 1px solid #fccd11;
    border-radius: .4rem
  }
</style>
